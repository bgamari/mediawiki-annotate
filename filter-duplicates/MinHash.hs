{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

newtype Bucket = Bucket Integer
               deriving (Show, Ord, Eq)

data Bag a = None
           | One a
           | Two (Bag a) (Bag a)
           deriving (Functor, Foldable, Traversable)

instance Monoid (Bag a) where
    mempty = None
    mappend = Two

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

bucketForPara :: KnownNat n => Projections n -> WordVec n -> Bucket
bucketForPara projs v =
    let fromBits :: [Bool] -> Bucket
        fromBits = Bucket . foldl' (.|.) 0 . zipWith toBit [0..]
          where
            toBit :: Int -> Bool -> Integer
            toBit n True  = bit n
            toBit n False = 0
    in fromBits $ map (\p -> dotWordVecs v p > 1) projs

opts :: Parser (FilePath, Double, FilePath, FilePath)
opts = (,,,)
    <$> option str (long "embeddings" <> short 'e' <> metavar "GLOVE" <> help "GloVe embeddings")
    <*> option auto (long "threshold" <> short 't' <> metavar "THRESH" <> help "Similarity threshold" <> value 0.9)
    <*> option str (long "output" <> short 'o' <> metavar "OUTPUT" <> help "Output duplicates file")
    <*> argument str (metavar "PARAGRAPHS" <> help "Paragraphs file")

main :: IO ()
main = do
    (embeddingFile, thresh, outputFile, parasFile) <- execParser $ info (helper <*> opts) mempty

    let toTuple :: Paragraph -> (ParagraphId, [Term])
        toTuple p = (paraId p, tokenise $ paraToText p)
    ncaps <- getNumCapabilities
    setNumCapabilities 1
    paras <- V.fromList . listStatus "read" 100000 . map toTuple . take 100000 . decodeCborList <$> BSL.readFile parasFile
    putStrLn $ "Read "++show (V.length paras)++" paragraphs"

    SomeWordEmbedding (embedding :: WordEmbedding n) <- readGlove embeddingFile
    projs <- randomProjections 10
    putStrLn "Read embeddings"

    setNumCapabilities ncaps
    let embeddedParas :: V.Vector (ParagraphId, [Term], WordVec n)
        embeddedParas = V.map embed paras
          where
            embed (pid, terms) =
                let v = subtractWordVec embeddingMean $ embedTerms embedding terms
                in (pid, terms, v)

        embeddingMean =
            scaleWordVec (recip $ realToFrac $ V.length paras)
            $ sumWordVecs $ map (embedTerms embedding . snd) $ V.toList paras
    print $ wordVecElems embeddingMean

    let partitions :: M.Map Bucket (V.Vector (ParagraphId, [Term]))
        partitions = partitionParas projs embeddedParas
    putStrLn "Bucket counts:"
    putStrLn $ unlines $ map show $ parMap rseq (fmap length) $ M.toList partitions

    let duplicates :: [(ParagraphId, ParagraphId)]
        duplicates = concat $ listStatus "dup-chunk" 1 $ withStrategy (parBuffer 1024 rdeepseq)
                     $ concat $ M.elems $ fmap (hashSimilarities thresh) partitions
    writeFile outputFile $ show duplicates

hashSimilarities :: Double -> V.Vector (ParagraphId, [Term]) -> [[(ParagraphId, ParagraphId)]]
hashSimilarities thresh paras =
    [ [ (pid1, pid2)
      | (pid2, s2) <- takeWhile (\(pid,_) -> pid < pid1) $ V.toList hashes
      , let (denom, num) = IS.unionIntersectSize s1 s2
            sim = realToFrac num / realToFrac denom
      , sim > thresh
      ]
    | (pid1, s1) <- listStatus "test" 100000 $ V.toList hashes
    ]
  where
    toHashes :: [Term] -> IS.IntSet
    toHashes toks =
        IS.fromAscList $ sort
        $ map hash $ toBigrams toks
    hashes :: V.Vector (ParagraphId, IS.IntSet)
    hashes = fmap (fmap toHashes) paras
