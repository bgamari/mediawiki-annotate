{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Debug.Trace
import Options.Applicative
import Control.Parallel.Strategies
import Data.Monoid
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import PageRank
import EdgeDocCorpus
import Graph
import Dijkstra
import GraphExpansion
import CAR.Types
import CAR.Types.CborList
import Codec.Serialise

edgeDocsPath :: Parser FilePath
edgeDocsPath = argument str (help "input EdgeDoc list path")

modes :: Parser (IO ())
modes = subparser
    $ command "page-rank"  (info (helper <*> pageRankMode) mempty)
   <> command "degree-centrality" (info (helper <*> degreeCentralityMode) mempty)
   <> command "graph-stats" (info (helper <*> graphStatsMode) mempty)
   <> command "distances" (info (helper <*> distancesMode) mempty)

main :: IO ()
main = do
    mode <- execParser $ info (helper <*> modes) mempty
    mode

readEdgeDocs :: FilePath -> IO [EdgeDoc]
readEdgeDocs inPath = do
    (Just (0::Int), edgeDocs) <- readCborList inPath
    return edgeDocs

pageRankMode :: Parser (IO ())
pageRankMode =
    run
      <$> option str (long "output" <> help "output path (CBOR eigenvector representation)")
      <*> edgeDocsPath
  where
    run :: FilePath -> FilePath -> IO ()
    run outPath inPath = do
        binGraph <- edgeDocsToBinaryGraph <$> readEdgeDocs inPath
        let graph :: Graph PageId Float
            graph = Graph $ fmap (HM.fromList . flip zip (repeat 0) . HS.toList) binGraph
            res = pageRank 0 graph
        writeFileSerialise outPath $ toEntries $ head $ drop 10 res

degreeCentralityMode :: Parser (IO ())
degreeCentralityMode =
    run <$> edgeDocsPath
  where
    run :: FilePath -> IO ()
    run inPath = do
        binGraph <- edgeDocsToBinaryGraph <$> readEdgeDocs inPath
        let graph :: Graph PageId Float
            graph = Graph $ fmap (HM.fromList . flip zip (repeat 0) . HS.toList) binGraph
            degreeCentralities = fmap HM.size $ getGraph graph
        print degreeCentralities

distancesMode :: Parser (IO ())
distancesMode =
    run
      <$> edgeDocsPath
  where
    run :: FilePath -> IO ()
    run inPath = do
        binGraph <- edgeDocsToBinaryGraph <$> readEdgeDocs inPath
        let graph :: Graph PageId (Sum Int)
            graph = Graph $ fmap (HM.fromList . flip zip (repeat 0) . HS.toList) binGraph

            distToMaybe (Finite x) = Just x
            distToMaybe Infinite   = Nothing

            distances :: [(PageId, HM.HashMap PageId (Maybe (Sum Int)))]
            distances =
                withStrategy (parBuffer 1000 rdeepseq)
                $ fmap (\n -> (n, fmap (distToMaybe . fst) $ dijkstra graph $ traceShow n n))
                $ HM.keys
                $ getGraph graph
        writeFileSerialise "distances.cbor" distances

graphStatsMode :: Parser (IO ())
graphStatsMode =
    run <$> edgeDocsPath
  where
    run inPath = do
        binGraph <- edgeDocsToBinaryGraph <$> readEdgeDocs inPath
        let graph :: Graph PageId Float
            graph = Graph $ fmap (HM.fromList . flip zip (repeat 0) . HS.toList) binGraph
            sumDegree = sum $ fmap HM.size $ getGraph graph
            avgDegree :: Double
            avgDegree = realToFrac sumDegree / realToFrac (HM.size $ getGraph graph)
        print $ "average degree: "++show avgDegree

