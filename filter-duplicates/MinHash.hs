{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

import Data.Bits
import Data.Char
import Data.Foldable
import Data.Monoid
import Data.Maybe
import Data.List
import Data.Hashable
import Control.DeepSeq
import Control.Monad (replicateM)
import GHC.TypeLits
import GHC.Conc

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as Sort
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as M
import Options.Applicative
import Control.Parallel.Strategies
import System.CPUTime
import System.Random.MWC
import System.Random.MWC.Distributions (standard)

import SimplIR.StopWords
import SimplIR.WordEmbedding
import SimplIR.WordEmbedding.GloVe
import NLP.Snowball

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

type Projections n = [WordVec n]

randomProjections :: KnownNat n => Int -> IO (Projections n)
randomProjections nProjs = withSystemRandom $ asGenST $ \gen -> do
    replicateM nProjs $ generateWordVec (const $ fmap realToFrac $ standard gen)

-- | Given a list of projections of an n-dimensional embedding space and a word
-- vector (such as accumulated word vectors of word in a paragraph), compute the
-- 'Bucket' to which the vector belongs.
bucketForPara :: KnownNat n => Projections n -> WordVec n -> Bucket
bucketForPara projs v =
    let fromBits :: [Bool] -> Bucket
        fromBits = Bucket . foldl' (.|.) 0 . zipWith toBit [0..]
          where
            toBit :: Int -> Bool -> Integer
            toBit n True  = bit n
            toBit n False = 0
    in fromBits $ map (\p -> dotWordVecs v p > 1) projs

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
    paras <- V.fromList . listStatus "read" 100000 . map toTuple . decodeCborList <$> BSL.readFile parasFile
    putStrLn $ "Read "++show (V.length paras)++" paragraphs"

    SomeWordEmbedding (embedding :: WordEmbedding n) <- readGlove embeddingFile
    projs <- randomProjections nProjections
    putStrLn "Read embeddings"
    setNumCapabilities ncaps

    let embeddedParas :: V.Vector (ParagraphId, [Term], WordVec n)
        embeddedParas = V.map embed uncenteredParas
          where
            embed (pid, terms, v) =
                (pid, terms, subtractWordVec embeddingMean v)

        uncenteredParas :: V.Vector (ParagraphId, [Term], WordVec n)
        uncenteredParas = V.map embed paras
          where
            embed (pid, terms) = (pid, terms, embedTerms embedding terms)

        !embeddingMean =
            scaleWordVec (recip $ realToFrac $ V.length paras)
            $ sumWordVecs
            $ withStrategy (parBuffer 256 rseq)
            $ map (sumWordVecs . map (\(_,_,v) -> v) . V.toList)
            $ chunksOf 100000 uncenteredParas
    print $ wordVecElems embeddingMean

    let partitions :: M.Map Bucket (V.Vector (ParagraphId, [Term]))
        !partitions = partitionParas projs embeddedParas
    putStrLn "Bucket counts:"
    putStrLn $ unlines $ map show $ parMap rseq (fmap length) $ M.toList partitions

    let duplicates :: [(ParagraphId, ParagraphId)]
        duplicates = concat $ listStatus "dup-chunk" 1000 $ withStrategy (parBuffer 1024 rdeepseq)
                     $ foldMap (hashSimilarities thresh) $ M.elems partitions
    writeFile outputFile $ show duplicates

-- | For all elements of a given bucket, compute the similarity and chunks which
-- we will parallelise pair-wise similarity computation over. (paras are all from
-- the same bucket). Only pairs with sufficintly high jaccard will be returned.
-- (Needs final testing on real bigrams)
hashSimilarities :: Double -> V.Vector (ParagraphId, [Term]) -> [[(ParagraphId, ParagraphId)]]
hashSimilarities thresh paras =
    [ [ (pid1, pid2)
      | (pid1, bigrams1) <- V.toList chunk
      , (pid2, bigrams2) <- takeWhile (\(pid,_) -> pid < pid1) $ V.toList bigramHashes
      , let (denom, num) = IS.unionIntersectSize bigrams1 bigrams2
            sim = realToFrac num / realToFrac denom
      , sim > thresh
      ]
    | chunk <- chunksOf 100 bigramHashes
    ]
  where
    toBigramHashes :: [Term] -> IS.IntSet
    toBigramHashes toks =
        IS.fromAscList $ sort
        $ map hash $ toBigrams toks
    -- for each paragraph: bigrams are hashed onto integers
    bigramHashes :: V.Vector (ParagraphId, IS.IntSet)
    !bigramHashes = fmap (fmap toBigramHashes) paras
