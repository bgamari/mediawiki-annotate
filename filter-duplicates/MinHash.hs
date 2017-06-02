{-# LANGUAGE DeriveTraversable #-}

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

newtype Bucket = Bucket Integer
               deriving (Show, Ord, Eq)

data Bag a = None
           | One a
           | Two (Bag a) (Bag a)
           deriving (Functor, Foldable, Traversable)

instance Monoid (Bag a) where
    mempty = None
    mappend = Two

partitionParas :: KnownNat n => WordEmbedding n -> Projections n
               -> V.Vector (ParagraphId, [Term]) -> M.Map Bucket [(ParagraphId, [Term])]
partitionParas embedding projs paras =
    fmap toList
    $ M.unionsWith (<>)
    $ listStatus "partition" 10
    $ withStrategy strat
    $ map chunkToBuckets
    $ chunksOf 10000 paras

  where
    chunkToBuckets :: V.Vector (ParagraphId, [Term]) -> M.Map Bucket (Bag (ParagraphId, [Term]))
    chunkToBuckets ps =
        M.fromListWith (<>)
        [ (bucketForPara embedding projs toks, One (pid, toks))
        | (pid, toks) <- V.toList ps
        ]
    strat :: Strategy [M.Map Bucket (Bag (ParagraphId, [Term]))]
    strat = parBuffer 256 $ evalTraversable $ evalTraversable r0

type Projections n = [WordVec n]

randomProjections :: KnownNat n => Int -> IO (Projections n)
randomProjections nProjs = withSystemRandom $ asGenST $ \gen -> do
    replicateM nProjs $ generateWordVec (const $ fmap realToFrac $ standard gen)

bucketForPara :: KnownNat n => WordEmbedding n -> Projections n -> [Term] -> Bucket
bucketForPara embedding projs toks =
    let v = embedTerms embedding toks
        fromBits :: [Bool] -> Bucket
        fromBits = Bucket . foldl' (.|.) 0 . zipWith toBit [0..]
          where
            toBit :: Int -> Bool -> Integer
            toBit n True  = bit n
            toBit n False = 0
    in fromBits $ map (\p -> dotWordVecs v p > 1) projs

opts :: Parser (FilePath, Double, FilePath)
opts = (,,)
    <$> option str (long "embeddings" <> short 'e' <> metavar "GLOVE" <> help "GloVe embeddings")
    <*> option auto (long "threshold" <> short 't' <> metavar "THRESH" <> help "Similarity threshold" <> value 0.9)
    <*> argument str (metavar "PARAGRAPHS" <> help "Paragraphs file")

main :: IO ()
main = do
    (embeddingFile, thresh, parasFile) <- execParser $ info (helper <*> opts) mempty

    let toTuple :: Paragraph -> (ParagraphId, [Term])
        toTuple p = (paraId p, tokenise $ paraToText p)
    ncaps <- getNumCapabilities
    setNumCapabilities 1
    paras <- V.fromList . listStatus "read" 100000 . map toTuple . decodeCborList <$> BSL.readFile parasFile
    putStrLn $ "Read "++show (V.length paras)++" paragraphs"

    SomeWordEmbedding embedding <- readGlove embeddingFile
    projs <- randomProjections 10
    putStrLn "Read embeddings"

    setNumCapabilities ncaps

    let partitions :: M.Map Bucket [(ParagraphId, [Term])]
        partitions = partitionParas embedding projs paras
    putStrLn "Bucket counts:"
    putStrLn $ unlines $ map (show . fmap length) $ M.toList partitions

    let duplicates :: [(ParagraphId, ParagraphId)]
        duplicates = concat $ withStrategy (parBuffer 1024 rdeepseq)
                     $ concat $ M.elems $ fmap (hashSimilarities thresh) partitions
    writeFile "duplicates" $ show duplicates

hashSimilarities :: Double -> [(ParagraphId, [Term])] -> [[(ParagraphId, ParagraphId)]]
hashSimilarities thresh paras =
    [ [ (pid1, pid2)
      | (pid2, s2) <- hashes
      , let (denom, num) = IS.unionIntersectSize s1 s2
            sim = realToFrac num / realToFrac denom
      , pid1 < pid2
      , sim > thresh
      ]
    | (pid1, s1) <- listStatus "test" 100000 hashes
    ]
  where
    toHashes :: [Term] -> IS.IntSet
    toHashes toks =
        IS.fromAscList $ sort
        $ map hash $ toBigrams toks
    hashes :: [(ParagraphId, IS.IntSet)]
    hashes = map (fmap toHashes) $ listStatus "hashes" 100000 paras
