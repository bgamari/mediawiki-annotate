{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Ord
import Data.Bits
import Data.Char
import Data.Maybe
import Data.Monoid
import Data.List
import Numeric
import Control.Parallel.Strategies

import Data.Hashable
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector.Algorithms.Heap as Sort
import Options.Applicative
import Control.Parallel.Strategies

import SimplIR.StopWords
import NLP.Snowball

import CAR.Types
import CAR.Utils
import qualified Data.ByteString.Lazy as BSL


type Term = T.Text
newtype Bloom = Bloom Integer
              deriving (Eq, Num, Bits)

instance Show Bloom where
    showsPrec _ (Bloom b) = showHex b

opts :: Parser (Double, FilePath)
opts = (,)
    <$> option auto (long "threshold" <> short 't' <> help "similarity threshold" <> value 0.9)
    <*> argument str (help "pages file" <> metavar "PAGES")

data BloomTree = Node !Bloom !(V.Vector BloomTree)
               | Leaf !DedupPara
               deriving (Show)

data DedupPara = DedupPara { dedupParaId     :: !ParagraphId
                           , dedupParaBloom  :: !Bloom
                           , dedupParaTokens :: [Term]
                           }
               deriving (Show)

parasToBloomTree :: Int -> V.Vector DedupPara -> BloomTree
parasToBloomTree fanout = go . V.map Leaf
  where
    go :: V.Vector BloomTree -> BloomTree
    go xs
      | V.length xs <= fanout = toNode xs
      | otherwise =
          go $ V.fromList $ map toNode $ chunksOf fanout xs

    toNode :: V.Vector BloomTree -> BloomTree
    toNode xs = Node (unionBlooms $ map treeBloom $ V.toList xs) xs

treeBloom :: BloomTree -> Bloom
treeBloom (Node b _) = b
treeBloom (Leaf (DedupPara _ b _)) = b

chunksOf :: Int -> V.Vector a -> [V.Vector a]
chunksOf n = go
  where
    go xs
      | V.length xs < n = [xs]
      | otherwise       =
        let (a,b) = V.splitAt n xs
        in a : go b

main :: IO ()
main = do
    (thresh, parasFile) <- execParser $ info (helper <*> opts) mempty
    paras <- decodeCborList <$> BSL.readFile parasFile

    let textToBloom :: [Term] -> Bloom
        textToBloom toks =
            toBloom [ pair
                    | pair <- toBigrams toks
                    ]

    let paras' :: V.Vector DedupPara
        paras' =
            V.fromList
            [ DedupPara (paraId para) (textToBloom toks) toks
            | para <- paras
            , let toks = tokenise $ paraToText para
            ]

    putStrLn "running"
    --bruteForce thresh paras'
    putStrLn "ich habe fertig"
    xs <- treeSearch thresh paras'
    writeFile "duplicates" $ show [ (a,b) | (a,b,_) <- xs ]
    putStrLn "ich habe fertig"

treeSearch :: Double -> V.Vector DedupPara -> IO [(ParagraphId, ParagraphId, Double)]
treeSearch thresh paras = do
    tree <- mkCompact $! parasToBloomTree 64 paras
    putStrLn "Built tree"
    return
      $ foldMap (\(para, dups) -> [ (dedupParaId para, dedupParaId dup, j)
                                  | (dup, j) <- dups ])
      $ withStrategy (parBuffer 64 $ evalTuple2 rseq $ evalList rseq)
      $ map (\para -> (para, para `duplicates` tree)) (V.toList paras)
  where
    mkCompact = return

    duplicates :: DedupPara -> BloomTree -> [(DedupPara, Double)]
    duplicates (DedupPara pid0 b0 terms0) = go
      where
        bigrams0 = HS.fromList $ toBigrams terms0

        go :: BloomTree -> [(DedupPara, Double)]
        go (Leaf p@(DedupPara pid b terms))
          | bloomJaccard b0 b > thresh
          , pid0 < pid
          , let j = jaccard bigrams0 (HS.fromList $ toBigrams terms)
          , j > thresh
          = [(p, j)]
        go (Node b children)
          | approxJaccard > thresh
          = foldMap go children
          where
            approxJaccard = realToFrac (popCount (b .&. b0)) / realToFrac (popCount b0)
        go _ = []

bruteForce :: Double -> V.Vector DedupPara
           -> IO [(ParagraphId, ParagraphId, Double, Double)]
bruteForce thresh paras =
    return [ (pidA, pidB, j, j')
           | DedupPara pidA bloomA bodyA <- V.toList paras
           , DedupPara pidB bloomB bodyB <- V.toList paras
           , pidA < pidB
           , let j = bloomJaccard bloomA bloomB
           , let j' = jaccard (HS.fromList $ toBigrams bodyA) (HS.fromList $ toBigrams bodyB)
           , j >= thresh
           , j' >= thresh
           ]

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

tokenise :: TL.Text -> [Term]
tokenise =
    stems English
    . killStopwords enInquery
    . map TL.toStrict
    . TL.words
    . TL.toCaseFold
    . TL.filter (not . isPunctuation)

unionBlooms :: [Bloom] -> Bloom
unionBlooms = foldl' (.|.) 0

toBloom :: Hashable a => [a] -> Bloom
toBloom = foldl' (.|.) 0 . map toBit
  where toBit x = bit $ hash x .&. 1023

toBigrams :: [Term] -> [(Term, Term)]
toBigrams = mapMaybe f . tails
  where
    f (x:y:_) = Just (x,y)
    f _ = Nothing

