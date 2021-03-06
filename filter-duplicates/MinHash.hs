{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

import Data.Bits
import Data.Foldable
import Data.Semigroup hiding (option)
import Data.List
import Data.Word
import Data.Hashable
import Control.Monad (when, replicateM, replicateM_)
import qualified Control.Lens as L
import Control.DeepSeq
import GHC.TypeLits
import GHC.Conc
import Control.Exception
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Control.Monad.ST
import System.IO

import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as Sort
import qualified Data.Map.Strict as M
import qualified Data.HashSet as HS
import Options.Applicative
import Control.Parallel.Strategies
import System.Random.MWC
import System.Random.MWC.Distributions (standard)

import SimplIR.WordEmbedding
import SimplIR.WordEmbedding.Parse

import CAR.Types
import CAR.ToolVersion
import CAR.Utils
import qualified CAR.FilterDuplicates.IntSet as IS
import CAR.FilterDuplicates.Utils

import Debug.Trace

-- | Identifier of a bucket (think: fingerprint by which paragraphs are
-- bucketized)
newtype Bucket = Bucket Integer
               deriving (Show, Ord, Eq)

data Bag a = None
           | One a
           | Two (Bag a) (Bag a)
           deriving (Functor, Foldable, Traversable)

instance Semigroup (Bag a) where
    (<>) = Two

instance Monoid (Bag a) where
    mempty = None
    mappend = Two

type Projections n = [WordVec n]

-- | Generate @n@ random splitting hyperplanes
genRandomProjections :: KnownNat n => Maybe Word32 -> Int -> IO (Projections n)
genRandomProjections Nothing nProjs = withSystemRandom $ asGenST $ randomProjections nProjs
genRandomProjections (Just seed) nProjs = return $ runST $ do
    gen <- initialize (V.singleton seed)
    randomProjections nProjs gen

randomProjections :: KnownNat n => Int -> GenST s -> ST s (Projections n)
randomProjections nProjs gen =
    replicateM nProjs $ generateWordVec (const $ fmap realToFrac $ standard gen)

-- | Partition paragraphs into buckets via random projections of the accumulated
-- word embedding vector.
partitionParas :: forall n. KnownNat n => Projections n
               -> V.Vector (ParagraphId, V.Vector Term, WordVec n)
               -> M.Map Bucket (V.Vector (ParagraphId, V.Vector Term))
partitionParas projs paras =
    withStrategy (parTraversable rseq)
    $ fmap (V.modify Sort.sort . V.fromList . toList) unsorted
  where
    unsorted =
          M.unionsWith (<>)
        $ listStatus "partition" 10
        $ withStrategy strat
        $ map chunkToBuckets
        $ chunksOf 10000 paras

    chunkToBuckets :: V.Vector (ParagraphId, V.Vector Term, WordVec n)
                   -> M.Map Bucket (Bag (ParagraphId, V.Vector Term))
    chunkToBuckets ps =
        M.fromListWith (<>)
        [ (bucketForPara projs v, One (pid, toks))
        | (pid, toks, v) <- V.toList ps
        ]

    strat :: Strategy [M.Map Bucket (Bag (ParagraphId, V.Vector Term))]
    strat = parBuffer 256 $ evalTraversable $ evalTraversable r0

-- | Given a list of projections of an n-dimensional embedding space and a word
-- vector (such as accumulated word vectors of word in a paragraph), compute the
-- 'Bucket' to which the vector belongs.
bucketForPara :: (KnownNat n)
              => Projections n -> WordVec n -> Bucket
bucketForPara projs v =
    let fromBits :: [Bool] -> Bucket
        fromBits = Bucket . foldl' (.|.) 0 . zipWith toBit [0..]
          where
            toBit :: Int -> Bool -> Integer
            toBit n True  = bit n
            toBit n False = 0
    in fromBits $ map (\p -> dotWordVecs v p > 1) projs

-- | Mean-center word embeddings
centerWordEmbedding :: (KnownNat n)
                    => V.Vector (ParagraphId, V.Vector Term, WordVec n)
                    -> V.Vector (ParagraphId, V.Vector Term, WordVec n)
centerWordEmbedding uncenteredParas =
    V.map center uncenteredParas
  where
    center (pid, terms, v) = (pid, terms, subtractWordVec embeddingMean v)
    !embeddingMean =
        scaleWordVec (recip $ realToFrac $ V.length uncenteredParas)   -- average = sum / N  -- component-wise!
        $ sumWordVecs                                     -- this and next lines: efficient sumWordVecs across all paras
        $ withStrategy (parBuffer 256 rseq)
        $ map (sumWordVecs . map (\(_,_,v) -> v) . V.toList)
        $ chunksOf 100000 uncenteredParas

data MatchOrDedup a = Match a a
                    | Dedup a
                    deriving (Functor)

matchFrom :: MatchOrDedup a -> a
matchFrom (Match x _) = x
matchFrom (Dedup x)   = x

matchTo :: MatchOrDedup a -> a
matchTo (Match _ x) = x
matchTo (Dedup x)   = x

-- | For all elements of a given bucket, compute the similarity and chunks which
-- we will parallelise pair-wise similarity computation over. (paras are all from
-- the same bucket). Only pairs with sufficintly high jaccard will be returned.
-- (Needs final testing on real bigrams)
hashSimilarities :: Double
                 -> MatchOrDedup (V.Vector (ParagraphId, V.Vector Term))
                 -> [(ParagraphId, ParagraphId)]
hashSimilarities thresh paras =
    [ (pid1, pid2)
    | (pid1, bigrams1, bigramHash1) <- V.toList $ matchFrom bigramHashes
    , (pid2, bigrams2, bigramHash2) <- takeWhile (inUpperTriangle pid1) $ V.toList $ matchTo bigramHashes
    , isBigramSimilar bigramHash1 bigramHash2
    , isJaccardSimilar bigrams1 bigrams2
    ]
  where
    inUpperTriangle pid1 (pid,_,_) = pid < pid1

    isBigramSimilar :: IS.IntSet -> IS.IntSet -> Bool
    isBigramSimilar bigrams1 bigrams2 =
        let (denom, num) = IS.unionIntersectSize bigrams1 bigrams2
            sim = realToFrac num / realToFrac denom
        in sim > thresh

    isJaccardSimilar bigrams1 bigrams2 =
        let realSim = jaccard bigrams1 bigrams2
        in realSim > thresh

    toBigramHashes :: (ParagraphId, V.Vector Term) -> (ParagraphId, HS.HashSet (Term,Term), IS.IntSet)
    toBigramHashes (pid, toks) =
        let hashes =
              IS.fromAscList $ sort $ map hash $ toBigrams $ V.toList toks
        in (pid, HS.fromList $ toBigrams $ V.toList toks, hashes)

    -- for each paragraph: bigrams are hashed onto integers
    bigramHashes :: MatchOrDedup (V.Vector (ParagraphId, HS.HashSet (Term, Term), IS.IntSet))
    !bigramHashes = fmap (fmap toBigramHashes) paras


opts :: Parser (FilePath, Maybe Word32, Double, Int, FilePath, Maybe FilePath, FilePath, Maybe FilePath)
opts = (,,,,,,,)
    <$> option str (long "embeddings" <> short 'e' <> metavar "GLOVE" <> help "GloVe embeddings")
    <*> optional (option auto (long "seed" <> metavar "SEED" <> help "PRNG seed value"))
    <*> option auto (long "threshold" <> short 't' <> metavar "THRESH" <> help "Similarity threshold" <> value 0.9)
    <*> option auto (long "projections" <> metavar "N" <> help "number of splitting hyperplanes for partitioning" <> value 10)
    <*> option str (long "output" <> short 'o' <> metavar "OUTPUT" <> help "Output duplicates file")
    <*> optional (option str (long "bucket-counts" <> short 'c' <> metavar "OUTPUT" <> help "Output bucket counts file"))
    <*> argument str (metavar "PARAGRAPHS" <> help "Paragraphs file")
    <*> optional (argument str (metavar "PARAGRAPHS" <> help "Paragraphs file"))

readPartitionParas :: forall n. KnownNat n
                   => WordEmbedding n
                   -> Projections n
                   -> FilePath
                   -> IO (M.Map Bucket (V.Vector (ParagraphId, V.Vector Term)))
readPartitionParas embedding projs parasFile = do
    let toTuple :: Paragraph -> (ParagraphId, V.Vector Term)
        toTuple p = (paraId p, V.fromList $ tokenise $ paraToText p)

    paras <- V.fromList
        . listStatus "read" 100000
        . internTerms (L.each . L._2 . L.each)
        . map toTuple
        <$> readParagraphsFile parasFile
    _ <- evaluate $ force paras
    putStrLn $ "Read "++show (V.length paras)++" paragraphs"

    -- Perform rough bucketing via word embedding
    let embeddedParas :: V.Vector (ParagraphId, V.Vector Term, WordVec n)
        !embeddedParas = centerWordEmbedding uncenteredParas

        uncenteredParas :: V.Vector (ParagraphId, V.Vector Term, WordVec n)
        uncenteredParas = V.map embed paras
          where
            embed (pid, terms) = (pid, terms, embedTerms embedding $ V.toList terms)
    putStrLn "Computed embedding"

    -- Compute minhash buckets
    let partitions :: M.Map Bucket (V.Vector (ParagraphId, V.Vector Term))
        !partitions = partitionParas projs embeddedParas
    putStrLn "Finished bucketing"
    return partitions

writeBucketCounts :: FilePath -> M.Map Bucket (V.Vector (ParagraphId, V.Vector Term)) -> IO ()
writeBucketCounts outFile partitions =
    writeFile outFile
        $ unlines
        $ map (\(Bucket a,b) -> unwords [show a, show b])
        $ parMap rseq (fmap length) $ M.toList partitions

main :: IO ()
main = do
    (embeddingFile, seed, thresh, nProjections, outputFile, bucketCountsFile, fromParasFile, mbToParasFile) <-
        execParser' 1 (helper <*> opts) mempty

    SomeWordEmbedding (embedding :: WordEmbedding n) <- readWordEmbedding embeddingFile
    projs <- genRandomProjections seed nProjections
    putStrLn "Read embeddings"

    partitions <- case mbToParasFile of
                    Nothing -> do
                        fromPartitions <- readPartitionParas embedding projs fromParasFile
                        return $ Dedup fromPartitions
                    Just toParasFile -> do
                        fromPartitions <- readPartitionParas embedding projs fromParasFile
                        toPartitions   <- readPartitionParas embedding projs toParasFile
                        return $ Match fromPartitions toPartitions
    traverse_ (`writeBucketCounts` matchFrom partitions) bucketCountsFile

    -- Finally compute all-pairs similarity
    withSharedFile outputFile WriteMode $ \outHdl -> do
        let worker :: Int -> [(ParagraphId, ParagraphId)] -> IO ()
            worker n dups = do
                when (n `mod` 1000 == 0) $ putStrLn ("Number of buckets processed: " <> show n)
                _ <- evaluate $ force dups
                withSharedHandle outHdl $ \h ->
                    hPutStrLn h $ unlines
                      [ unpackParagraphId a <> "\t" <> unpackParagraphId b
                      | (a, b) <- dups ]

        ncaps <- getNumCapabilities
        parMapIOUnordered ncaps (uncurry worker)
            $ zip [0..]
            $ fmap (hashSimilarities' thresh)
            $ M.elems
            $ alignPartitions partitions

alignPartitions :: MatchOrDedup (M.Map Bucket (V.Vector (ParagraphId, V.Vector Term)))
                -> M.Map Bucket (MatchOrDedup (V.Vector (ParagraphId, V.Vector Term)))
alignPartitions (Dedup x) = fmap Dedup x
alignPartitions (Match from to) = M.intersectionWith Match from to

hashSimilarities' :: Double
                  -> MatchOrDedup (V.Vector (ParagraphId, V.Vector Term))
                  -> [(ParagraphId, ParagraphId)]
hashSimilarities' thresh paras
  | numInBucket > 100 =
    trace (   "% false positives = " <> show (100.0 * (1.0 - (realToFrac numMatches / realToFrac numPairs)))
           <> " =| num pairs     = " <> show numPairs
           <> " =| num matches   = " <> show numMatches
           <> " =| bucket size   = " <> show numInBucket)
          matches
  | otherwise = matches
  where
    matches = hashSimilarities thresh paras
    numInBucket = V.length $ matchFrom paras
    numPairs = (numInBucket^2 - numInBucket) `div` 2
    numMatches = length matches

newtype SharedHandle = SharedHandle (TMVar Handle)

withSharedFile :: FilePath -> IOMode
               -> (SharedHandle -> IO a) -> IO a
withSharedFile path mode =
    bracket (openSharedFile path mode) closeSharedFile

openSharedFile :: FilePath -> IOMode -> IO SharedHandle
openSharedFile path mode =
    SharedHandle <$> (openFile path mode >>= newTMVarIO)

closeSharedFile :: SharedHandle -> IO ()
closeSharedFile (SharedHandle hdl) =
    atomically (takeTMVar hdl) >>= hClose

withSharedHandle :: SharedHandle -> (Handle -> IO a) -> IO a
withSharedHandle (SharedHandle var) =
    bracket (atomically $ takeTMVar var) (atomically . putTMVar var)

parMapIOUnordered :: (Semigroup b, Monoid b)
                  => Int
                  -> (a -> IO b)
                  -> [a]
                  -> IO b
parMapIOUnordered n f xs = do
    ahead <- atomically $ newTSem $ fromIntegral n
    accum <- newTVarIO mempty
    forM_ xs $ \x -> do
        atomically $ waitTSem ahead
        worker <- async $ do
            r <- f x
            atomically $ do
                r0 <- readTVar accum
                writeTVar accum $! r0 <> r
            atomically $ signalTSem ahead
        link worker

    replicateM_ n $ atomically $ readTVar accum
    atomically $ readTVar accum
