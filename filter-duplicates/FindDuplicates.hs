{-# LANGUAGE BangPatterns #-}

import Data.Char
import Data.Maybe
import Data.Monoid
import Data.List

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import qualified Data.HashSet as HS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as BSS
import Options.Applicative
import Control.Parallel.Strategies
import System.IO

import SimplIR.StopWords
import NLP.Snowball

--import Bloom.Naive
import Bloom.Opt
--import Bloom.IntSet
import CAR.Types
import CAR.Utils
import Debug.Trace

type Term = T.Text

opts :: Parser (Double, FilePath, Maybe Int, Int, FilePath)
opts = (,,,,)
    <$> option auto (long "threshold" <> short 't' <> help "similarity threshold" <> value 0.9)
    <*> option str (long "output" <> short 'o' <> help "output file name")
    <*> optional (option auto (long "num" <> short 'n' <> help "number of paragraphs to compute duplicates with"))
    <*> option auto (long "fanout" <> help "bloom tree fanout" <> value 64)
    <*> argument str (help "pages file" <> metavar "PAGES")

data BloomTree = Node !Bloom !(V.Vector BloomTree)
               | Leaf !DedupPara
               deriving (Show)

data DedupPara = DedupPara { dedupParaId     :: !ParagraphId
                           , dedupParaBloom  :: !Bloom
                           , dedupParaTokens :: [Term]
                           }
               deriving (Show)

toDedupPara :: ParagraphId -> [Term] -> DedupPara
toDedupPara pid toks =
    --traceShow (pid, toks, toBloom $ toBigrams toks) $
    DedupPara pid (toBloom $ toBigrams toks) toks

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

treeDepth :: BloomTree -> Int
treeDepth (Node _ children) = V.maximum (V.map treeDepth children) + 1
treeDepth _ = 1

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
    (thresh, outputFile, maybeNumParas, fanout, parasFile) <- execParser $ info (helper <*> opts) mempty
    paras <- decodeCborList <$> BSL.readFile parasFile

    let paras' :: V.Vector DedupPara
        paras' =
            V.fromList
            [ toDedupPara (paraId para) toks
            | para <- paras
            , let toks = tokenise $ paraToText para
            ]
    --let paras' = V.fromList testParagraphs

    let tree = parasToBloomTree fanout paras'
    tree `seq` putStrLn "Built tree"
    print $ treeDepth tree
    print $ V.length paras'
    let xs = treeSearch thresh tree paras'
    writeFile outputFile $ show [ (a,b) | (a,b,_) <- xs ]
    putStrLn "ich habe fertig"
    hFlush stderr
    hFlush stdout

listStatus :: [a] -> [a]
listStatus = go 0
  where
    go n (x:xs)
      | n `mod` 100 == 0 = traceShow n (x : go (n+1) xs)
      | otherwise         = x : go (n+1) xs
    go _ [] = []

treeSearch :: Double -> BloomTree -> V.Vector DedupPara -> [(ParagraphId, ParagraphId, Double)]
treeSearch thresh bloomTree paras =
      foldMap (\(para, dups) -> [ (dedupParaId para, dedupParaId dup, j)
                                | (dup, j) <- dups ])
    $ listStatus
    $ withStrategy (parBuffer 256 $ evalTuple2 rseq $ evalList rseq)
    $ map (\para -> (para, para `duplicates` bloomTree)) (V.toList paras)
  where
    traceShow _ x = x

    duplicates :: DedupPara -> BloomTree -> [(DedupPara, Double)]
    duplicates (DedupPara pid0 b0 terms0) = traceShow ("go", pid0) . go 0
      where
        bigrams0 = HS.fromList $ toBigrams terms0

        go :: Int -> BloomTree -> [(DedupPara, Double)]
        go !i (Leaf p@(DedupPara pid b terms))
          | traceShow ("test0", bloomJaccard b0 b, pid) $ bloomJaccard b0 b > thresh
          , pid0 < pid
          , let j = jaccard bigrams0 (HS.fromList $ toBigrams terms)
          , traceShow ("test", j) $ j > thresh
          = traceShow ("hit", i)
            [(p, j)]
        go i (Node b children)
          | traceShow ("node", approxJaccard) $ approxJaccard > thresh
          = traceShow ("node", i, approxJaccard) $
            foldMap (go (i+1)) children
          where approxJaccard = boundedJaccard b0 b
        go i _ = []

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

toBigrams :: [Term] -> [(Term, Term)]
toBigrams = mapMaybe f . tails
  where
    f (x:y:_) = Just (x,y)
    f _ = Nothing

testParagraphs :: [DedupPara]
testParagraphs = zipWith toTestPara [1..]
    [ "love hate love hate"
    , "love hate love love"
    , "love chip date love"
    , "chip date love love"
    ]
  where
    toTestPara n t = toDedupPara pid toks
      where
        pid = ParagraphId $ BSS.pack $ map (fromIntegral . ord) $ "test-"++show n
        toks = map T.pack $ words t
