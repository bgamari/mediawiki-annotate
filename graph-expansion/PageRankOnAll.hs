{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

import Debug.Trace
import Options.Applicative
import Control.Parallel.Strategies
import Data.Monoid
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector.Indexed as VI
import qualified Data.Vector.Unboxed as VU
import Data.Vector.Unboxed.Deriving
import PageRank
import EdgeDocCorpus
import Graph
import DenseMapping
import Dijkstra
import GraphExpansion
import CAR.Types
import CAR.Types.CborList
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

readEdgeDocGraph :: Num a => FilePath -> IO (Graph PageId a)
readEdgeDocGraph inPath = do
    binGraph <- edgeDocsToBinaryGraph <$> readEdgeDocs inPath
    return $ Graph $ fmap (HM.fromList . flip zip (repeat 1) . HS.toList) binGraph

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
        writeFileSerialise outPath $ toEntries $ head $ drop 10 res

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

            distances :: [(PageId, VI.Vector VU.Vector (DenseId PageId) (Distance (Sum Int)))]
            distances =
                withStrategy (parBuffer 1000 rdeepseq)
                $ fmap (\n -> (n, denseDijkstra mapping graph $ traceShow n n))
                $ HM.keys
                $ getGraph graph
        print distances

graphStatsMode :: Parser (IO ())
graphStatsMode =
    run <$> edgeDocsPath
  where
    run inPath = do
        graph <- readEdgeDocGraph @Int inPath
        let sumDegree = sum $ fmap HM.size $ getGraph graph

            avgDegree :: Double
            avgDegree = realToFrac sumDegree / realToFrac (HM.size $ getGraph graph)
        print $ "average degree: "++show avgDegree
