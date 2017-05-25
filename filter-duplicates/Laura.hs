import Data.Ord
import Data.Bits
import Data.Char
import Data.Maybe
import Data.List
import Numeric

import Data.Hashable
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector.Algorithms.Heap as Sort

import SimplIR.StopWords
import NLP.Snowball

import CAR.Types
import CAR.Utils
import qualified Data.ByteString.Lazy as BSL

type Term = T.Text
type Bloom = Integer

main :: IO ()
main = do
    let parasFile = "train.test200.cbor.paragraphs"
    stats <- toCorpusStats . decodeCborList <$> BSL.readFile parasFile
    let nPairs = HM.size stats
        sortedStats :: [((Term, Term), Int)]
        sortedStats =
            sortBy (flip $ comparing snd) $ HM.toList stats

        mostFreq :: HS.HashSet (Term, Term)
        mostFreq = HS.fromList [] -- $ map fst $ take (nPairs `div` 2) sortedStats
    putStrLn $ unlines $ map show $ HS.toList mostFreq

    paras <- decodeCborList <$> BSL.readFile parasFile

    let textToBloom :: [Term] -> Bloom
        textToBloom toks =
            toBloom [ pair
                    | pair <- toBigrams toks
                    --, not $ pair `HS.member` mostFreq
                    ]

    let paras' :: [(ParagraphId, [Term], Bloom)]
        paras' =
            [ (paraId para, toks, textToBloom toks)
            | para <- paras
            , let toks = tokenise $ paraToText para
            ]

    --mapM_ print [ (pidA, pidB, j)
    --            | (pidA, bodyA) <- paras'
    --            , (pidB, bodyB) <- paras'
    --            , pidA < pidB
    --            , let pairsA = HS.fromList $ toBigrams $ bodyA
    --            , let pairsB = HS.fromList $ toBigrams $ bodyB
    --            , let j = jaccard pairsA pairsB
    --            , j >= 0.95
    --            ]

    {-
    mapM_ print [ (pidA, j, j', showHex bloomA "", bodyA)
                | (pidA, bodyA) <- paras'
                , let bloomA = textToBloom bodyA
                , let j' = bloomJaccard bloomA bloomA
                , let pairsA = HS.fromList $ toBigrams $ bodyA
                      j = jaccard pairsA pairsA
                ]
-}

    let thresh = 0.5
    mapM_ print [ (pidA, pidB, j, j')
                | (pidA, bodyA, bloomA) <- paras'
                , (pidB, bodyB, bloomB) <- paras'
                , pidA < pidB
                , let j = bloomJaccard bloomA bloomB
                , let j' = jaccard (HS.fromList $ toBigrams bodyA) (HS.fromList $ toBigrams bodyB)
                , j >= thresh
                , j' >= thresh
                ]

    putStrLn "ich habe fertig"

bloomJaccard :: Bloom -> Bloom -> Double
bloomJaccard a b =
    realToFrac (popCount (a .&. b)) / realToFrac (popCount (a .|. b))

jaccard :: HS.HashSet (Term, Term) -> HS.HashSet (Term, Term) -> Double
jaccard xs ys
  | denom == 0 = 0
  | otherwise  = num / denom
  where
    num = realToFrac $ HS.size (xs `HS.intersection` ys)
    denom = realToFrac $ HS.size (xs `HS.union` ys)

toCorpusStats :: [Paragraph] -> HM.HashMap (Term, Term) Int
toCorpusStats paras =
    HM.fromListWith (+)
    [ (x,1) | para <- paras, x <- toBigrams $ tokenise $ paraToText para ]

tokenise :: TL.Text -> [Term]
tokenise =
    stems English
    . killStopwords enInquery
    . map TL.toStrict
    . TL.words
    . TL.toCaseFold
    . TL.filter (not . isPunctuation)

toBloom :: Hashable a => [a] -> Bloom
toBloom = foldl' (.|.) 0 . map toBit
  where toBit x = bit $ hash x .&. 1023

toBigrams :: [Term] -> [(Term, Term)]
toBigrams = mapMaybe f . tails
  where
    f (x:y:_) = Just (x,y)
    f _ = Nothing

