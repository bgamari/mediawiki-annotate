import Data.Bits
import Data.Char
import Data.Monoid
import Data.Maybe
import Data.List
import Data.Hashable
import Control.DeepSeq
import Control.Monad (replicateM)
import GHC.TypeLits

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

type Term = T.Text

newtype Bucket = Bucket Int
               deriving (Show, Ord, Eq)

partitionParas :: KnownNat n => WordEmbedding n -> Projections n
               -> [(ParagraphId, [Term])] -> M.Map Bucket [(ParagraphId, [Term])]
partitionParas embedding projs paras =
    M.fromListWith (++)
    [ (bucketForPara embedding projs toks, [(pid, toks)])
    | (pid, toks) <- paras
    ]

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
            toBit :: Int -> Bool -> Int
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

    paras <- decodeCborList <$> BSL.readFile parasFile
    let paras' = [ (paraId para, toks)
                 | para <- paras
                 , let toks = tokenise $ paraToText para
                 ]

    SomeWordEmbedding embedding <- readGlove embeddingFile
    projs <- randomProjections 10
    let partitions :: M.Map Bucket [(ParagraphId, [Term])]
        partitions = partitionParas embedding projs paras'

    let filterDuplicates ps =
            [ (a, b)
            | (a, b, sim) <- minHashSimilarities ps
            , sim > thresh
            ]
    print $ fmap filterDuplicates partitions

minHashSimilarities :: [(ParagraphId, [Term])] -> [(ParagraphId, ParagraphId, Double)]
minHashSimilarities paras =
    [ (pid1, pid2, sim)
    | (pid1, s1) <- minHashes
    , (pid2, s2) <- minHashes
    , let (denom, num) = IS.unionIntersectSize s1 s2
          sim = realToFrac num / realToFrac denom
    , pid1 < pid2
    ]
  where
    k = 200
    salts = [56, 77]
    minHash :: [Term] -> IS.IntSet
    minHash toks =
        IS.fromAscList $ take k $ sort
        [ hashWithSalt salt bigram
        | bigram <- toBigrams toks
        , salt <- salts
        ]
    minHashes :: [(ParagraphId, IS.IntSet)]
    minHashes = [ (pid, minHash toks)
                | (pid, toks) <- paras
                ]

tokenise :: TL.Text -> [Term]
tokenise =
    stems English
    . killStopwords enInquery
    . map TL.toStrict
    . TL.words
    . TL.toCaseFold
    . TL.filter (not . isPunctuation)

toBigrams :: [Term] -> [(Term, Term)]
toBigrams = mapMaybe f . tails
  where
    f (x:y:_) = Just (x,y)
    f _ = Nothing
