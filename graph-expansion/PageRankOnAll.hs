{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

import GHC.Conc
import Control.DeepSeq
import Data.Coerce
import Data.Foldable
import qualified Control.Foldl as Foldl
import Debug.Trace
import Options.Applicative
import Control.Parallel.Strategies
import Data.Semigroup hiding (option)
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
import SimplIR.Utils.Compact
import Codec.Serialise

$(derivingUnbox "Sum"
     [t| forall a. VU.Unbox a => Sum a -> a |]
     [| \(Sum n) -> n |]
     [| Sum |]
 )

data Mean a = Mean !Int !a
            deriving (Show, Functor)

instance NFData (Mean a) where
    rnf x = x `seq` ()
instance Num a => Semigroup (Mean a) where
    Mean n x <> Mean m y = Mean (n+m) (x+y)
instance Num a => Monoid (Mean a) where
    mempty = Mean 0 0
    mappend = (<>)

one :: a -> Mean a
one = Mean 1

getMean :: Fractional a => Mean a -> a
getMean (Mean n x) = x / fromIntegral n

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
        let degreeHist :: HM.HashMap Int Int
            degreeHist = coerce @(HM.HashMap Int (Sum Int)) @(HM.HashMap Int Int)
                         $ HM.fromListWith (<>)
                         $ map (\neighs -> (HM.size neighs, Sum 1))
                         $ HM.elems $ getGraph graph
        putStrLn $ unlines [ unwords [show bin, show n]
                           | (bin, n) <- HM.toList degreeHist
                           ]
        let avgDegree :: Double
            avgDegree = getMean $ fmap realToFrac $ foldMap (\(bin,n) -> Mean n bin) (HM.toList degreeHist)
        putStrLn $ "average degree: "++show avgDegree
