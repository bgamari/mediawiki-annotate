{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import GHC.Conc
import Debug.Trace
import Options.Applicative
import Control.Parallel.Strategies
import Data.Ord
import Data.Monoid
import Data.Foldable
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

main :: IO ()
main = do
    mode <- execParser $ info (helper <*> modes) mempty
    mode

readEdgeDocs :: FilePath -> IO [EdgeDoc]
readEdgeDocs inPath = do
    ncaps <- getNumCapabilities
    setNumCapabilities 1
    (Just (0::Int), edgeDocs) <- inCompactM $ readCborList inPath
    setNumCapabilities ncaps
    return edgeDocs

readEdgeDocGraph :: Num a => FilePath -> IO (Graph PageId a)
readEdgeDocGraph inPath = do
    binGraph <- edgeDocsToBinaryGraph <$> readEdgeDocs inPath
    putStrLn $ "Read graph of "++show (HM.size binGraph)++" nodes"
    return $ Graph $ fmap (HM.fromList . flip zip (repeat 1) . HS.toList) binGraph

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

        let mapRanking = Seq.sortBy (comparing carScore)
                         . fmap fixScore
            fixScore :: Run.EntityRankingEntry -> Run.EntityRankingEntry
            fixScore e
              | Just s <- M.lookup (carDocument e) pageRanks
              = e { carScore = realToFrac s }
              | otherwise
              = error $ "Couldn't find pagerank score for " ++ show (carDocument e)

        Run.writeEntityRun outFile $ foldMap (toList . mapRanking) rankings

degreeCentralityMode :: Parser (IO ())
degreeCentralityMode =
    run <$> edgeDocsPath
  where
    run :: FilePath -> IO ()
    run inPath = do
        graph <- readEdgeDocGraph @Int inPath
        let degreeCentralities = fmap HM.size $ getGraph graph
        print degreeCentralities

distancesMode :: Parser (IO ())
distancesMode =
    run
      <$> edgeDocsPath
  where
    run :: FilePath -> IO ()
    run inPath = do
        graph <- readEdgeDocGraph @(Sum Int) inPath

        let mapping = mkDenseMapping $ nodeSet graph

            --folds :: Foldl.Fold ((Distance (Sum Int), _)) (Mean (Distance Int), Max (Distance Int))
            --folds =
            --    Foldl.premap (fmap getSum . fst)
            --    $ (,) <$> Foldl.premap one Foldl.mconcat <*> Foldl.premap Max Foldl.mconcat

            distances :: [(PageId, VI.Vector VU.Vector (DenseId PageId) (Distance (Sum Int)))]
            distances =
                  fmap (\n -> (n, denseDijkstra mapping graph $ traceShow n n))
                $ HM.keys
                $ getGraph graph
        print $ withStrategy (parBuffer 1000 $ evalTuple2 r0 rdeepseq)
              $ fmap (fmap $ filter (/= Finite 0) . VI.elems) distances

graphStatsMode :: Parser (IO ())
graphStatsMode =
    run <$> edgeDocsPath
  where
    run inPath = do
        graph <- readEdgeDocGraph @Int inPath
        let folds = (,,) <$> Foldl.minimum <*> Foldl.maximum <*> Foldl.mean
            (Just minDeg, Just maxDeg, avgDeg) = Foldl.fold (Foldl.premap (realToFrac . HM.size) folds) (getGraph graph)
        print $ "average degree: "++show (avgDeg :: Double)

        -- degree histogram
        let hist = histogramFoldable @1000 (logBinning (minDeg, maxDeg))
                   $ fmap (realToFrac . HM.size) $ toList $ getGraph graph
        writeFile "degree-hist.txt" $ unlines
            [ show (l :: Double) ++ "\t" ++ show n
            | ((l,_), n) <- binCounts hist
            ]
