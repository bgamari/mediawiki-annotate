{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}


import Control.Exception (bracket)
import Control.DeepSeq
import Control.Monad (when)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Data.Monoid hiding (All, Any)
import Data.Foldable
import Data.Coerce
import Data.List (intercalate)
import Options.Applicative
import System.IO
import Data.Time.Clock
import Numeric
import GHC.Generics

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL

import CAR.Types
import qualified ExtractKnowledgeBase as KB

--import qualified Control.Concurrent.ForkMap as ForkMap
import WriteRanking
import Retrieve
import GraphExpansion

opts :: Parser (FilePath, FilePath, FilePath, [Method])
opts =
    (,,,)
    <$> argument str (help "articles file" <> metavar "ANNOTATIONS FILE")
    <*> option str (short 'q' <> long "outlines file" <> metavar "FILE" <> help "Outline file (queries)")
    <*> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")
    <*> (some (option method $ short 'm' <> long "method" <> metavar "METHOD" <> help "Methods to run")
         <|> pure allMethods)
    where
      method = (methodMap M.!) <$> str
      methodMap = M.fromList [ (showMethodName m, m) | m <- allMethods ]


data QueryDoc = QueryDoc { queryDocQueryId :: PageId
                         , queryDocQueryText :: PageName
                         , queryDocLeadEntities ::  HS.HashSet PageId
                         , queryDocRawTerms :: [Term]
                         }
           deriving Show

pagesToLeadEntities :: [Page] -> [QueryDoc]
pagesToLeadEntities pages =
        map (\page -> let kbDoc = KB.transformContent inlinkCounts page
                      in QueryDoc { queryDocQueryId        = KB.kbDocPageId kbDoc
                                  , queryDocQueryText      = pageName page
                                  , queryDocLeadEntities   = HS.fromList $ fmap pageNameToId $ KB.kbDocOutLinks kbDoc
                                  , queryDocRawTerms       = textToTokens' $ getPageName $ pageName page
                                  }
            )
        $ pages
      where
        inlinkInfo   = KB.collectInlinkInfo pages
        inlinkCounts = KB.resolveRedirects inlinkInfo

type Name = String

data GraphStats = GraphStats { nNodes, nEdges :: !Int }
                deriving (Show)



-- ----------------------------------------------------------------------

data EdgeDocWithScores = EdgeDocWithScores { withScoreEdgeDoc   :: EdgeDoc
                                           , withScoreCount     :: Int
                                           , withScoreScore     :: Double
                                           , withScoreRank      :: Int
                                           }
           deriving (Show, Generic)
instance NFData EdgeDocWithScores


rankNormDocs :: RankingFunction -> Int -> Int -> [Term] -> [EdgeDoc] -> [EdgeDocWithScores]
rankNormDocs rankDocs normRank cutoffRank query edgeDocs =
    let rankedEdgeDocs :: [(EdgeDoc, Double)]
        rankedEdgeDocs = take cutoffRank
                         $ rankDocs query
                         $ fmap (\edgeDoc -> (edgeDoc, edgeDocContent $ edgeDoc))
                         $ edgeDocs
        (_, normScore)
          | length rankedEdgeDocs > normRank  = rankedEdgeDocs !! normRank
          | not (null rankedEdgeDocs)         = last rankedEdgeDocs
          | otherwise                         = error "rankNormDocs: ranking empty"

        cutRankedEdgeDocs =  fmap (\(rank, (edgeDoc, score))  -> (EdgeDocWithScores edgeDoc 1 (exp (score - normScore)) (rank)))
                           $ zip [1::Int ..]
                           $ rankedEdgeDocs
    in cutRankedEdgeDocs


filterGraphByTop100GraphEdges :: RankingFunction -> [Term] -> [EdgeDoc] ->  HM.HashMap PageId [EdgeDocWithScores]
filterGraphByTop100GraphEdges rankDocs query edgeDocs =
        let edges :: [EdgeDocWithScores]
            edges  = rankNormDocs rankDocs 100 100 query edgeDocs
        in HM.fromListWith (++) $ foldMap groupByEntity $ edges
  where groupByEntity :: EdgeDocWithScores -> [(PageId, [EdgeDocWithScores])]
        groupByEntity elem@(EdgeDocWithScores edgeDoc _ _ _) =
                  [ (entity, [elem])
                  | entity <- edgeDocNeighbors $ edgeDoc]


filterGraphByTop5NodeEdges :: RankingFunction -> [Term] -> [EdgeDoc] ->  HM.HashMap PageId [EdgeDocWithScores]
filterGraphByTop5NodeEdges rankDocs query edgeDocs =
  let perNodeEdges :: HM.HashMap PageId [EdgeDoc]
      perNodeEdges = HM.fromListWith (++) $ foldMap groupByEntity edgeDocs
      perNodeFilteredEdges :: HM.HashMap PageId [EdgeDocWithScores]
      perNodeFilteredEdges =  fmap filterNode perNodeEdges
   in perNodeFilteredEdges


  where groupByEntity :: EdgeDoc -> [(PageId, [EdgeDoc])]
        groupByEntity edgeDoc =
                  [ (entity, [edgeDoc])
                  | entity <- edgeDocNeighbors $ edgeDoc]
        filterNode :: [EdgeDoc] -> [EdgeDocWithScores]
        filterNode edgeDocs' =
            rankNormDocs rankDocs 5 5 query edgeDocs'


noFilterTwice :: [EdgeDoc] ->  HM.HashMap PageId (HM.HashMap PageId [EdgeDoc])
noFilterTwice edgeDocs =
  let perSourceEdges :: HM.HashMap PageId [EdgeDoc]
      perSourceEdges = HM.fromListWith (++) $ foldMap groupByEntity edgeDocs
      perTargetEdges = fmap (\edgeDocs' -> HM.fromListWith (++) $ foldMap groupByEntity edgeDocs' ) $ perSourceEdges
  in perTargetEdges
  where groupByEntity :: EdgeDoc -> [(PageId, [EdgeDoc])]
        groupByEntity edgeDoc =
                  [ (entity, [edgeDoc])
                  | entity <- edgeDocNeighbors $ edgeDoc]


random100Filter :: [EdgeDoc] ->  HM.HashMap PageId (HM.HashMap PageId [EdgeDoc])
random100Filter edgeDocs =
  let edgeDocs' = take 100 $ HS.toList $ HS.fromList edgeDocs -- rely on HashSet randomizing the list
      perSourceEdges :: HM.HashMap PageId [EdgeDoc]
      perSourceEdges = HM.fromListWith (++) $ foldMap groupByEntity edgeDocs'
      perTargetEdges = fmap (\edgeDocs' -> HM.fromListWith (++) $ foldMap groupByEntity edgeDocs' ) $ perSourceEdges
  in perTargetEdges
  where groupByEntity :: EdgeDoc -> [(PageId, [EdgeDoc])]
        groupByEntity edgeDoc =
                  [ (entity, [edgeDoc])
                  | entity <- edgeDocNeighbors $ edgeDoc]


accumulateEdgeWeights :: forall w. Num w => HM.HashMap PageId [EdgeDocWithScores] -> (EdgeDocWithScores -> w) ->   HM.HashMap PageId (HM.HashMap PageId w)
accumulateEdgeWeights sourceToEdgeDocsWithScores by=
    HM.mapWithKey countEdgeDocs sourceToEdgeDocsWithScores
  where countEdgeDocs :: PageId -> [EdgeDocWithScores] -> HM.HashMap PageId w
        countEdgeDocs sourceNode edgeDocsWithScores =
            HM.fromListWith (+)
              [ (targetNode, by edgeDocsWithScore)
              | edgeDocsWithScore <- edgeDocsWithScores
              , targetNode <- edgeDocNeighbors $ withScoreEdgeDoc $ edgeDocsWithScore
              , targetNode /= sourceNode
              ]



-- Marginalized over second argument in edges, e.g. Map source (Map target weight_{st}) -> Map source weight_{s*}
marginalizeEdges :: HM.HashMap PageId (HM.HashMap PageId Double) -> [(PageId, Double)]
marginalizeEdges graph =
    HM.toList $ fmap marginalizeMap $ graph
  where marginalizeMap :: HM.HashMap PageId Double -> Double
        marginalizeMap map =
            sum $ fmap snd $ HM.toList $ map



-- ----------------------------------------------------------------------

data GraphNames = Top5PerNode | Top100PerGraph | SimpleGraph | RandomGraph
    deriving (Show, Enum, Bounded, Ord, Eq, Generic)
data WeightingNames = Count | Binary | Score | RecipRank | LinearRank| BucketRank
    deriving (Show, Enum, Bounded, Ord, Eq, Generic)
data GraphRankingNames = PageRank | ShortPath | MargEdges
    deriving (Show, Enum, Bounded, Ord, Eq, Generic)
data Method = Method GraphNames WeightingNames GraphRankingNames
    deriving ( Ord, Eq, Generic)
instance Show Method where
    show = showMethodName
showMethodName:: Method -> String
showMethodName (Method a b c) = intercalate "-" [show a, show b, show c]

allMethods :: [Method]
allMethods = [ Method gName wName rName
             | gName <- [minBound :: GraphNames .. maxBound]
             , wName <- [minBound :: WeightingNames .. maxBound]
             , rName <- [minBound :: GraphRankingNames .. maxBound]
             ]

instance NFData GraphNames
instance NFData WeightingNames
instance NFData GraphRankingNames


type RankingFunction = forall elem. [Term] -> [(elem, T.Text)] -> [(elem, Double)]

computeRankingsForQuery :: RankingFunction
                        -> QueryDoc -> Int -> UniverseGraph -> BinarySymmetricGraph
                           -> [(Method, [(PageId, Double)])]

computeRankingsForQuery rankDocs queryDoc radius universeGraph binarySymmetricGraph =
    let seeds :: HS.HashSet PageId
        seeds = queryDocLeadEntities $ queryDoc
        query =  queryDocRawTerms $ queryDoc

        nodeSet = expandNodesK binarySymmetricGraph seeds radius

        universeSubset ::  HM.HashMap PageId [EdgeDoc]
        universeSubset = subsetOfUniverseGraph universeGraph nodeSet


        edgeDocsSubset :: [EdgeDoc]
        edgeDocsSubset = HS.toList $ HS.fromList $ concat $ HM.elems universeSubset

        fancyGraphs :: [(GraphNames, HM.HashMap PageId [EdgeDocWithScores])]
        fancyGraphs = [(Top5PerNode,  filterGraphByTop5NodeEdges rankDocs query $ edgeDocsSubset)
                      ,(Top100PerGraph,  filterGraphByTop100GraphEdges rankDocs query $ edgeDocsSubset)
                      ]

        simpleGraphs :: [(GraphNames, HM.HashMap PageId (HM.HashMap PageId [EdgeDoc]))]
        simpleGraphs =  [(SimpleGraph, noFilterTwice $ edgeDocsSubset)
                        ,(RandomGraph, random100Filter $ edgeDocsSubset)
                        ]

        weightings :: [(WeightingNames, EdgeDocWithScores -> Double)]
        weightings =  [ (Count,  realToFrac . withScoreCount)
                      , (Score, withScoreScore)
                      , (RecipRank,   (\edge ->  1.0 / (realToFrac $ withScoreRank edge )))
                      , (LinearRank,  (\edge -> realToFrac (101 - (withScoreRank edge))))
                      , (BucketRank,  (\edge ->  let rank = withScoreRank $ edge
                                                 in if rank <= 5 then 3.0 else
                                                    if rank <= 20 then 2.0 else
                                                    1.0
                                      ))
                      ]

        graphRankings :: [(GraphRankingNames, (HM.HashMap PageId (HM.HashMap PageId Double) -> [(PageId, Double)]))]
        graphRankings = [(PageRank, \graph ->  let wgraph = toGraph graph
                                               in rankByPageRank wgraph 0.15 20)
                        ,(ShortPath, \graph -> let wgraph = toGraph graph
                                               in rankByShortestPaths (fmap (max $ Sum 0.001) $ coerce wgraph) (toList seeds))
                        ,(MargEdges, \graph -> marginalizeEdges graph)
                        ]

        fancyWeightedGraphs ::  [((GraphNames, WeightingNames), HM.HashMap PageId (HM.HashMap PageId Double))]
        fancyWeightedGraphs =  [((gname, wname), accumulateEdgeWeights graph weighting)
                               | (gname, graph) <- fancyGraphs
                               , (wname, weighting) <- weightings
                               ]

        simpleWeightedGraphs :: [((GraphNames, WeightingNames), HM.HashMap PageId (HM.HashMap PageId Double))]
        simpleWeightedGraphs = concat [
                                         [ ((gname, Count), fmap (fmap (realToFrac . length) )  $ graph)
                                         , ((gname, Binary),      fmap (fmap (const 1))         $ graph)
                                         ]
                                      | (gname, graph) <- simpleGraphs]


        computeRankings' :: [(Method,  [(PageId, Double)])]
        computeRankings' =
            [ (Method gname wname rname,  graphRanking (graph) )
            | ((gname, wname), graph) <- fancyWeightedGraphs ++ simpleWeightedGraphs,
              (rname, graphRanking) <- graphRankings
            ]

    in (fancyGraphs, simpleGraphs) `deepseq` computeRankings'

main :: IO ()
main = do
    (articlesFile, queryFile, outputFilePrefix, runMethods) <- execParser $ info (helper <*> opts) mempty
    pagesForLinkExtraction <- decodeCborList <$> BSL.readFile articlesFile
    putStrLn $ "# Running methods: " ++ show runMethods


    let universeGraph :: UniverseGraph
        !universeGraph = edgeDocsToUniverseGraph $ emitEdgeDocs pagesForLinkExtraction

    let binarySymmetricGraph :: BinarySymmetricGraph
        !binarySymmetricGraph = universeToBinaryGraph universeGraph


    queriesToSeedEntities <- pagesToLeadEntities . decodeCborList <$> BSL.readFile queryFile

    let queryTermsAll = foldMap (queryDocRawTerms) $ queriesToSeedEntities
    putStrLn $ "# queryTermsAll " ++ show queryTermsAll

    let !corpusStatistics = Retrieve.computeTermCounts queryTermsAll
                          $ map (\edgeDoc -> Doc edgeDoc (edgeDocContent edgeDoc))
                          $ emitEdgeDocs pagesForLinkExtraction

    putStrLn $ "# corpus statistics " ++ show corpusStatistics

    let rankDoc q docs =
            map (\(Doc a b) -> (a,b))
            $ Retrieve.retrieve corpusStatistics q
            $ map (uncurry Doc) docs

    handles <- sequence $ M.fromList  -- todo if we run different models in parallel, this will overwrite previous results.
      [ (method, openFile (outputFilePrefix ++ showMethodName method ++ ".run") WriteMode)
      | method <- runMethods ]
        :: IO (M.Map Method Handle)

    ncaps <- getNumCapabilities
    let --forM_' = forM_
        forM_' = forConcurrentlyN_ ncaps
        --forM_' xs f = void $ runEffect $ ForkMap.mapIO 16 16 f xs
    forM_' queriesToSeedEntities $ \query -> do
        when (null $ queryDocLeadEntities query) $
            T.putStr $ T.pack $ "# Query with no lead entities: "++show query++"\n"

        T.putStr $ T.pack $ "# Processing query "++ show query++"\n"
        let queryId = queryDocQueryId query
            rankings = computeRankingsForQuery rankDoc query 3
                                  universeGraph binarySymmetricGraph

            runMethod :: Method -> [(PageId, Double)] -> IO ()
            runMethod method ranking = do
                let hdl = handles M.! method

                let logMsg t = T.putStr $ T.pack $ unpackPageId queryId++"\t"++showMethodName method++"\t"++t++"\n"
                    logTimed t doIt = do
                        logMsg t
                        t0 <- getCurrentTime
                        !r <- doIt
                        t1 <- getCurrentTime
                        let dt = t1 `diffUTCTime` t0
                        logMsg $ t++"\ttime="++(showFFloat (Just 3) (realToFrac dt / 60 :: Double) "")
                        return r

                logMsg $ "evaluating"
                --logTimed "evaluating graph" $ evaluate $ rnf graph
                --logMsg $ "graph size: "++show (graphSize graph)
                --ranking <- logTimed "computing ranking" $ evaluate $ force $ computeRanking graph
                logMsg $ "ranking entries="++show (length ranking)
                let formatted = WriteRanking.formatEntityRankings
                                (T.pack $ show method)
                                (T.pack $ unpackPageId queryId)
                                ranking
                logTimed "writing ranking" $ TL.hPutStr hdl formatted

        let badMethods = S.fromList (map fst rankings) `S.difference` M.keysSet handles
        when (not $ S.null badMethods) $ fail $ "unknown methods: "++show badMethods
        mapM_ (uncurry runMethod) $ filter (\(m,_) -> m `elem` runMethods) rankings

    mapM_ hClose handles

forConcurrentlyN_ :: Foldable f => Int -> f a -> (a -> IO ()) -> IO ()
forConcurrentlyN_ n xs f = do
    sem <- atomically $ newTSem n
    let run x = bracket (atomically $ waitTSem sem) (const $ atomically $ signalTSem sem) (const $ f x)
    forConcurrently_ xs run
