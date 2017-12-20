{-# LANGUAGE BangPatterns #-}

import Data.Char
import Data.Monoid

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.HashSet as HS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as BSS
import Options.Applicative
import Control.Parallel.Strategies
import System.IO
import System.CPUTime

--import Bloom.Naive
import Bloom.Opt
--import Bloom.IntSet
import CAR.Types
import CAR.Utils
import Utils


-- ------------------------------------------------
--     Not used!                                 --
-- ------------------------------------------------

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

main :: IO ()
main = do
    (thresh, outputFile, maybeNumParas, fanout, parasFile) <- execParser $ info (helper <*> opts) mempty
    paras <- readParagraphsFile parasFile

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
    startTime <- getCPUTime
    putStrLn $ "Tree building time: "++show (realToFrac startTime / 1e12)
    let xs = treeSearch thresh tree (maybe id V.take maybeNumParas $ paras')
    writeFile outputFile $ show [ (a,b) | (a,b,_) <- xs ]
    endTime <- getCPUTime
    putStrLn "ich habe fertig"
    putStrLn $ "Search time: "++show (realToFrac (endTime - startTime) / 1e12)
    hFlush stderr
    hFlush stdout

treeSearch :: Double -> BloomTree -> V.Vector DedupPara -> [(ParagraphId, ParagraphId, Double)]
treeSearch thresh bloomTree paras =
      foldMap (\(para, dups) -> [ (dedupParaId para, dedupParaId dup, j)
                                | (dup, j) <- dups ])
    $ listStatus "search" 10000
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
          | traceShow ("leaf", i, bloomJaccard b0 b, pid) $
            bloomJaccard b0 b > thresh
          , pid0 < pid
          , let j = jaccard bigrams0 (HS.fromList $ toBigrams terms)
          , j > thresh
          = traceShow ("hit", i, j)
            [(p, j)]
        go i (Node b children)
          | traceShow ("node", i, approxJaccard) $ approxJaccard > thresh
          = foldMap (go (i+1)) children
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
