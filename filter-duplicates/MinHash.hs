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

partitionParas :: KnownNat n => WordEmbedding n -> Projections n
               -> V.Vector (ParagraphId, [Term]) -> M.Map Bucket [(ParagraphId, [Term])]
partitionParas embedding projs paras =
    M.unionsWith (++)
    $ withStrategy strat
    $ map chunkToBuckets
    $ listStatus "partition" 10
    $ chunksOf 10000 paras

  where
    chunkToBuckets :: V.Vector (ParagraphId, [Term]) -> M.Map Bucket [(ParagraphId, [Term])]
    chunkToBuckets ps =
        M.fromListWith (++)
        [ (bucketForPara embedding projs toks, [(pid, toks)])
        | (pid, toks) <- V.toList ps
        ]
    strat :: Strategy [M.Map Bucket [(ParagraphId, [Term])]]
    strat = parBuffer 256 rseq

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
    setNumCapabilities 1
    paras <- V.fromList . map toTuple . decodeCborList <$> BSL.readFile parasFile
    putStrLn $ "Read "++show (V.length paras)++" paragraphs"
    setNumCapabilities 30

    SomeWordEmbedding embedding <- readGlove embeddingFile
    projs <- randomProjections 10
    putStrLn "Read embeddings"

    let partitions :: M.Map Bucket [(ParagraphId, [Term])]
        partitions = partitionParas embedding projs paras
    putStrLn "Bucket counts:"
    putStrLn $ unlines $ map (show . fmap length) $ M.toList partitions

    let filterDuplicates :: [(ParagraphId, [Term])] -> [(ParagraphId, ParagraphId)]
        filterDuplicates ps =
            [ (a, b)
            | (a, b, sim) <- hashSimilarities ps
            , sim > thresh
            ]
    let duplicates :: [(ParagraphId, ParagraphId)]
        duplicates = fold $ withStrategy (parTraversable rdeepseq) $ fmap filterDuplicates partitions
    print duplicates

hashSimilarities :: [(ParagraphId, [Term])] -> [(ParagraphId, ParagraphId, Double)]
hashSimilarities paras =
    [ (pid1, pid2, sim)
    | (pid1, s1) <- listStatus "test" 100 hashes
    , (pid2, s2) <- hashes
    , let (denom, num) = IS.unionIntersectSize s1 s2
          sim = realToFrac num / realToFrac denom
    , pid1 < pid2
    ]
  where
    toHashes :: [Term] -> IS.IntSet
    toHashes toks =
        IS.fromAscList $ sort
        $ map hash $ toBigrams toks
    hashes :: [(ParagraphId, IS.IntSet)]
    hashes = map (fmap toHashes) $ listStatus "hashes" 1000 paras
