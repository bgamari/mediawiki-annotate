{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

import Data.Bits
import Data.Foldable
import Data.Monoid
import Data.List
import Data.Hashable
import Control.Monad (when, replicateM, replicateM_)
import Control.DeepSeq
import GHC.TypeLits
import GHC.Conc
import Control.Exception
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
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
import CAR.Utils
import qualified IntSet as IS
import Utils

-- | Identifier of a bucket (think: fingerprint by which paragraphs are
-- bucketized)
newtype Bucket = Bucket Integer
               deriving (Show, Ord, Eq)

data Bag a = None
           | One a
           | Two (Bag a) (Bag a)
           deriving (Functor, Foldable, Traversable)

instance Monoid (Bag a) where
    mempty = None
    mappend = Two

type Projections n = [WordVec n]

-- | Generate @n@ random splitting hyperplanes
randomProjections :: KnownNat n => Int -> IO (Projections n)
randomProjections nProjs = withSystemRandom $ asGenST $ \gen ->
    replicateM nProjs $ generateWordVec (const $ fmap realToFrac $ standard gen)

-- | Partition paragraphs into buckets via random projections of the accumulated
-- word embedding vector.
partitionParas :: forall n. KnownNat n => Projections n
               -> V.Vector (ParagraphId, [Term], WordVec n)
               -> M.Map Bucket (V.Vector (ParagraphId, [Term]))
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

    chunkToBuckets :: V.Vector (ParagraphId, [Term], WordVec n)
                   -> M.Map Bucket (Bag (ParagraphId, [Term]))
    chunkToBuckets ps =
        M.fromListWith (<>)
        [ (bucketForPara projs v, One (pid, toks))
        | (pid, toks, v) <- V.toList ps
        ]

    strat :: Strategy [M.Map Bucket (Bag (ParagraphId, [Term]))]
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
                    => V.Vector (ParagraphId, [Term], WordVec n)
                    -> V.Vector (ParagraphId, [Term], WordVec n)
centerWordEmbedding uncenteredParas =
    V.map center uncenteredParas
  where
    center (pid, terms, v) = (pid, terms, subtractWordVec embeddingMean v)
    !embeddingMean =
        scaleWordVec (recip $ realToFrac $ V.length uncenteredParas)
        $ sumWordVecs
        $ withStrategy (parBuffer 256 rseq)
        $ map (sumWordVecs . map (\(_,_,v) -> v) . V.toList)
        $ chunksOf 100000 uncenteredParas

-- | For all elements of a given bucket, compute the similarity and chunks which
-- we will parallelise pair-wise similarity computation over. (paras are all from
-- the same bucket). Only pairs with sufficintly high jaccard will be returned.
-- (Needs final testing on real bigrams)
hashSimilarities :: Double -> V.Vector (ParagraphId, [Term]) -> [[(ParagraphId, ParagraphId)]]
hashSimilarities thresh paras =
    [ [ (pid1, pid2)
      | (pid1, toks1, bigrams1) <- V.toList chunk
      , (pid2, toks2, bigrams2) <- takeWhile (\(pid,_,_) -> pid < pid1) $ V.toList bigramHashes
      , let (denom, num) = IS.unionIntersectSize bigrams1 bigrams2
            sim = realToFrac num / realToFrac denom
      , sim > thresh
      , let realSim = jaccard (HS.fromList $ toBigrams toks1) (HS.fromList $ toBigrams toks2)
      , realSim > thresh
      ]
    | chunk <- chunksOf 100 bigramHashes
    ]
  where
    toBigramHashes :: (ParagraphId, [Term]) -> (ParagraphId, [Term], IS.IntSet)
    toBigramHashes (pid, toks) =
        let hashes =
              IS.fromAscList $ sort $ map hash $ toBigrams toks
        in (pid, toks, hashes)
    -- for each paragraph: bigrams are hashed onto integers
    bigramHashes :: V.Vector (ParagraphId, [Term], IS.IntSet)
    !bigramHashes = fmap toBigramHashes paras


opts :: Parser (FilePath, Double, Int, FilePath, FilePath)
opts = (,,,,)
    <$> option str (long "embeddings" <> short 'e' <> metavar "GLOVE" <> help "GloVe embeddings")
    <*> option auto (long "threshold" <> short 't' <> metavar "THRESH" <> help "Similarity threshold" <> value 0.9)
    <*> option auto (long "projections" <> metavar "N" <> help "number of splitting hyperplanes for partitioning" <> value 10)
    <*> option str (long "output" <> short 'o' <> metavar "OUTPUT" <> help "Output duplicates file")
    <*> argument str (metavar "PARAGRAPHS" <> help "Paragraphs file")

main :: IO ()
main = do
    (embeddingFile, thresh, nProjections, outputFile, parasFile) <-
        execParser $ info (helper <*> opts) mempty

    let toTuple :: Paragraph -> (ParagraphId, [Term])
        toTuple p = (paraId p, tokenise $ paraToText p)
    ncaps <- getNumCapabilities
    setNumCapabilities 1
    paras <- V.fromList . listStatus "read" 100000 . map toTuple <$> readParagraphsFile parasFile
    putStrLn $ "Read "++show (V.length paras)++" paragraphs"

    SomeWordEmbedding (embedding :: WordEmbedding n) <- readWordEmbedding embeddingFile
    projs <- randomProjections nProjections
    putStrLn "Read embeddings"
    setNumCapabilities ncaps

    let !embeddedParas = centerWordEmbedding uncenteredParas

        uncenteredParas :: V.Vector (ParagraphId, [Term], WordVec n)
        uncenteredParas = V.map embed paras
          where
            embed (pid, terms) = (pid, terms, embedTerms embedding terms)

    putStrLn "Computed embedding"

    -- First compute minhash buckets
    let partitions :: M.Map Bucket (V.Vector (ParagraphId, [Term]))
        !partitions = partitionParas projs embeddedParas
    putStrLn "Bucket counts:"
    putStrLn $ unlines $ map show $ parMap rseq (fmap length) $ M.toList partitions

    -- Finally compute all-pairs similarity
    withSharedFile outputFile WriteMode $ \outHdl -> do
        let worker :: Int -> [(ParagraphId, ParagraphId)] -> IO ()
            worker n dups = do
                when (n `mod` 1000 == 0) $ print n
                _ <- evaluate $ force dups
                withSharedHandle outHdl $ \h ->
                    hPutStrLn h $ unlines
                      [ unpackParagraphId a <> "\t" <> unpackParagraphId b
                      | (a, b) <- dups ]

        parMapIOUnordered ncaps (uncurry worker) $ zip [0..]
            $ foldMap (hashSimilarities thresh) $ M.elems partitions


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

parMapIOUnordered :: Monoid b
                  => Int
                  -> (a -> IO b)
                  -> [a]
                  -> IO b
parMapIOUnordered n f xs = do
    ahead <- atomically $ newTSem n
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
