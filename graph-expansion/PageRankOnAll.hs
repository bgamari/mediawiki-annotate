{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import GHC.Conc
import Debug.Trace
import Options.Applicative
import Control.Parallel.Strategies
import Data.Either
import Data.Foldable
import Data.List
import Data.Monoid
import Data.Ord
import qualified Control.Foldl as Foldl
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector.Indexed as VI
import qualified Data.Vector.Unboxed as VU
import qualified Data.Sequence as Seq
import Data.Vector.Unboxed.Deriving
import PageRank
import EdgeDocCorpus
import Graph
import DenseMapping
import Dijkstra
import GraphExpansion
import SimplIR.Histogram
import CAR.RunFile as Run
import CAR.Types
import CAR.Types.CborList
import SimplIR.Utils.Compact
import Codec.Serialise

$(derivingUnbox "Sum"
     [t| forall a. VU.Unbox a => Sum a -> a |]
     [| \(Sum n) -> n |]
     [| Sum |]
 )

edgeDocsPath :: Parser FilePath
edgeDocsPath = argument str (help "input EdgeDoc list path")

modes :: Parser (IO ())
modes = subparser
    $ command "page-rank"  (info (helper <*> pageRankMode) (progDesc "Compute PageRank on a graph"))
   <> command "degree-centrality" (info (helper <*> degreeCentralityMode) mempty)
   <> command "graph-stats" (info (helper <*> graphStatsMode) mempty)
   <> command "distances" (info (helper <*> distancesMode) mempty)
   <> command "rerank" (info (helper <*> rerankMode) (progDesc "Rerank a run file with results from a PageRank run"))
   <> command "dump-it" (info (helper <*> dumpIt) (progDesc "Rerank a run file with results from a PageRank run"))

main :: IO ()
main = do
    mode <- execParser $ info (helper <*> modes) mempty
    mode

singleThreaded :: IO a -> IO a
singleThreaded m = do
    ncaps <- getNumCapabilities
    setNumCapabilities 1
    r <- m
    r `seq` setNumCapabilities ncaps
    return r

readEdgeDocs :: FilePath -> IO [EdgeDoc]
readEdgeDocs inPath = do
    (Just (0::Int), edgeDocs) <- readCborList inPath
    return edgeDocs

readEdgeDocGraph :: (Num a, NFData a) => FilePath -> IO (Graph PageId a)
readEdgeDocGraph inPath = singleThreaded $ do
    binGraph <- edgeDocsToBinaryGraph <$> readEdgeDocs inPath
    putStrLn $ "Read graph of "++show (HM.size binGraph)++" nodes"
    return $! inCompact $ Graph $ HM.map (\xs -> 1 <$ HS.toMap xs) binGraph

type PageRankScores = [(PageId, Float)]

pageRankMode :: Parser (IO ())
pageRankMode =
    run
      <$> option str (long "output" <> help "output path (CBOR eigenvector representation)")
      <*> edgeDocsPath
  where
    run :: FilePath -> FilePath -> IO ()
    run outPath inPath = do
        graph <- readEdgeDocGraph @Float inPath
        let res = pageRank 0 graph
            contents :: PageRankScores
            contents = toEntries $ head $ drop 10 res
        writeFileSerialise outPath contents

rerankMode :: Parser (IO ())
rerankMode =
    run
      <$> option str (long "output" <> short 'o' <> metavar "RUN" <> help "output run file")
      <*> argument str (metavar "RUN" <> help "retrieval run file")
      <*> argument str (metavar "PAGERANK" <> help "PageRank run")
  where
    run :: FilePath -> FilePath -> FilePath -> IO ()
    run outFile runFile pageRankRunFile = do
        rankings <- groupByQuery <$> Run.readEntityRun runFile
        pageRanks <- M.fromList <$> readFileDeserialise pageRankRunFile
            :: IO (M.Map PageId Float)

        let fixScore :: Run.EntityRankingEntry -> Either PageId Run.EntityRankingEntry
            fixScore e
              | Just s <- M.lookup (carDocument e) pageRanks
              = Right e { carScore = realToFrac s }
              | otherwise
              = Left (carDocument e)

        let rankings' :: M.Map QueryId (Seq.Seq (Either PageId EntityRankingEntry))
            rankings' = fmap (fmap fixScore) rankings
        Run.writeEntityRun outFile $ foldMap (sortBy (flip $ comparing carScore) . rights . toList) rankings'
        putStrLn $ "Missing entities:\n"++unlines (map show $ foldMap (lefts . toList) rankings')

degreeCentralityMode :: Parser (IO ())
degreeCentralityMode =
    run <$> edgeDocsPath
  where
    run :: FilePath -> IO ()
    run inPath = do
        graph <- readEdgeDocGraph @Int inPath
        let degreeCentralities = fmap HM.size $ getGraph graph
        print degreeCentralities

takeEvery :: Int -> [a] -> [a]
takeEvery n = go
  where go [] = []
        go (x:xs) = x : go (drop n xs)

distancesMode :: Parser (IO ())
distancesMode =
    run
      <$> edgeDocsPath
  where
    run :: FilePath -> IO ()
    run inPath = do
        graph <- readEdgeDocGraph @(Sum Int) inPath

        let !mapping = mkDenseMapping $ nodeSet graph

        putStrLn "mapping evaluated"

        ncaps <- getNumCapabilities
        let --folds :: Foldl.Fold ((Distance (Sum Int), _)) (Mean (Distance Int), Max (Distance Int))
            --folds =
            --    Foldl.premap (fmap getSum . fst)
            --    $ (,) <$> Foldl.premap one Foldl.mconcat <*> Foldl.premap Max Foldl.mconcat

            distances :: [(PageId, VI.Vector VU.Vector (DenseId PageId) (Distance (Sum Int)))]
            distances =
                  fmap (\n -> (n, denseDijkstra mapping graph $ traceShow n n))
                $ takeEvery 5000
                $ HM.keys
                $ getGraph graph

            distances' :: [(PageId, [(Int, Count)])]
            distances' =
                withStrategy (parBuffer (ncaps+4) $ evalTuple2 r0 $ evalTraversable $ evalTuple2 rseq rseq)
                $ map (fmap $ nonEmptyBinCounts . histogramFoldable binning . map clampDist . VI.elems) distances
              where
                binning = integralBinning @22 0

                clampDist (Finite e)
                    | e > 20       = 21
                    | otherwise    = getSum e
                clampDist Infinite = 22

        --print $ withStrategy (parBuffer 1000 $ evalTuple2 r0 rdeepseq)
        --      $ fmap (fmap $ filter (/= Finite 0) . VI.elems) distances
        writeCborList "distances.cbor" () distances'

readDistances :: FilePath -> IO [(PageId, [(Int, Count)])]
readDistances = fmap snd . readCborList @()

graphStatsMode :: Parser (IO ())
graphStatsMode =
    run <$> edgeDocsPath
  where
    run inPath = do
        graph <- readEdgeDocGraph @Int inPath
        let folds = (,,) <$> Foldl.minimum <*> Foldl.maximum <*> Foldl.mean
            (Just minDeg, Just maxDeg, avgDeg) =
                Foldl.fold (Foldl.premap (realToFrac . HM.size) folds) (getGraph graph)
        print $ "average degree: "++show (avgDeg :: Double)

        writeFile "degree.txt" $ unlines $ map (show . HM.size) $ toList $ getGraph graph

        -- degree histogram
        let hist = histogramFoldable @1000 (logBinning (minDeg, maxDeg))
                   $ fmap (realToFrac . HM.size) $ toList $ getGraph graph
        writeFile "degree-hist.txt" $ unlines
            [ show (l :: Double) ++ "\t" ++ show n
            | ((l,_), n) <- binCounts hist
            ]

dumpIt :: Parser (IO ())
dumpIt =
    run <$> argument str mempty
  where
    run inPath = do
        xs <- readDistances inPath
        mapM_ print
            [ Foldl.fold ((,) <$> Foldl.length <*> Foldl.mean)
              [ realToFrac n :: Double | (bin, n) <- ys ]
            | (_, ys) <- xs
            ]
        --print $ Foldl.fold ((,) <$> Foldl.length <*> Foldl.mean) [ realToFrac n :: Double | Just n <- xs >>= snd ]

