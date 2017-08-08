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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}

import Control.Exception (bracket)
import Control.Monad (when, void)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Data.Maybe
import Data.Tuple
import Data.Semigroup hiding (All, Any, option)
import Data.Foldable
import Data.Coerce
import Data.Bifunctor
import Options.Applicative
import System.IO
import Data.Time.Clock
import Numeric
import GHC.TypeLits
import Data.Aeson

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

import qualified Data.GraphViz as Dot
import qualified Data.GraphViz.Printing as Dot
import qualified Data.GraphViz.Attributes.Complete as Dot
import qualified Data.GraphViz.Commands.IO as Dot
import CAR.Types
import CAR.AnnotationsFile as AnnsFile
import CAR.Retrieve as Retrieve
import CAR.Utils

import EdgeDocCorpus
import WriteRanking
import GraphExpansion
import GraphExpansionExperiments
import SimplIR.WordEmbedding
import SimplIR.WordEmbedding.GloVe
import qualified SimplIR.SimpleIndex as Index
import qualified SimplIR.SimpleIndex.Models.QueryLikelihood as QL
import qualified SimplIR.SimpleIndex.Models.BM25 as BM25
import ZScore

import Debug.Trace

data QuerySource = QueriesFromCbor FilePath
                 | QueriesFromJson FilePath


data Graphset = Fullgraph | Subgraph

opts :: Parser ( FilePath, FilePath, FilePath, QuerySource
               , Maybe [Method], Int, Index.OnDiskIndex Term EdgeDoc Int
               , Graphset
               , Maybe PageId , Maybe FilePath)
opts =
    (,,,,,,,,,)
    <$> argument str (help "articles file" <> metavar "ANNOTATIONS FILE")
    <*> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")
    <*> option str (short 'e' <> long "embedding" <> metavar "FILE" <> help "Glove embeddings file")
    <*> querySource
    <*> optional methods
    <*> option auto (long "hops" <> metavar "INT" <> help "number of hops for initial outward expansion" <> value 3)
    <*> option (Index.OnDiskIndex <$> str)
               (short 'i' <> long "index" <> metavar "INDEX" <> help "simplir edgedoc index")
    <*> flag Fullgraph Subgraph (long "fullgraph" <> help "Run on full graph, not use subgraph retrieval")
    <*> optional (option (packPageId <$> str) (long "query" <> metavar "QUERY" <> help "execute only this query"))
    <*> optional (option str (long "dot" <> metavar "FILE" <> help "export dot graph to this file"))
    where
      methods :: Parser [Method]
      methods = fmap concat $ some
          $ option (fmap (\x->[x]) method) (short 'm' <> long "method" <> metavar "METHOD" <> help "Methods to run")
        <|> option (str >>= methodSet) (long "method-set" <> metavar "METHODSET" <> help "Set of methods to run")

      method :: ReadM Method
      method = do name <- str
                  case M.lookup name methodMap of
                    Just m  -> return m
                    Nothing -> fail $ unlines $ [ "unknown method "++name
                                                , "known methods:"
                                                ] ++ map ("  "++) (M.keys methodMap)

      methodSet :: String -> ReadM [Method]
      methodSet "all"  = pure allMethods
      methodSet "topn" = pure topNPerGraphMethods
      methodSet "core" = pure coreMethods
      methodSet "base" = pure baseMethods
      methodSet "prio0" = pure prio0Methods
      methodSet "prio1" = pure prio1Methods
      methodSet "prio2" = pure prio2Methods
      methodSet "prio3" = pure prio3Methods
      methodSet "prio4" = pure prio4Methods
      methodSet "fix1" = pure fix1Methods
      methodSet "fix2" = pure fix2Methods
      methodSet "test" = pure testMethods
      methodSet "candidateset" = pure [CandidateSet]
      methodSet _      = fail "unknown method set"

      methodMap = M.fromList [ (showMethodName m, m) | m <- allMethods ]

      querySource =
              option (fmap QueriesFromCbor str) (short 'q' <> long "queries" <> metavar "CBOR" <> help "Queries from CBOR pages")
          <|> option (fmap QueriesFromJson str) (short 'j' <> long "queries-json" <> metavar "JSON" <> help "Queries from JSON")


candidateSetList :: HS.HashSet PageId -> Int -> BinarySymmetricGraph
                ->  [(PageId, Double)]
candidateSetList seeds radius binarySymmetricGraph  =
    let nodeSet :: HS.HashSet PageId
        nodeSet = expandNodesK binarySymmetricGraph seeds radius
   in [ (pageId, 1.0) | pageId <- HS.toList nodeSet]



nodesToAttributes :: forall n. (KnownNat n)
                 => AnnotationsFile
                 -> WordEmbedding n
                 -> HS.HashSet PageId
                 -> HM.HashMap PageId (Attributes (EmbeddingDim n))
nodesToAttributes annsFile wordEmbedding nodeSet =
    zScoreStandardize
    $ foldMap (\pid -> HM.singleton pid $ toWordVec pid) (toList nodeSet)
  where
    toWordVec pid =
        wordVecToAttributes
        $ maybe (pageNameEmbeddingAttributes wordEmbedding pid) (pageTextEmbeddingAttributes wordEmbedding)
        $ AnnsFile.lookupPage pid annsFile

computeRankingsForQuery :: forall n. (KnownNat n)
                        => (Index.RetrievalModel Term EdgeDoc Int -> RetrievalFunction EdgeDoc)
                        -> AnnotationsFile
                        -> PageId ->  [Term] -> HS.HashSet PageId -> Int -> UniverseGraph -> BinarySymmetricGraph
                        -> WordEmbedding n
                        -> (PageId -> PageId)
                        -> [(Method, [(PageId, Double)])]

computeRankingsForQuery retrieveDocs annsFile queryPageId query seeds radius universeGraph binarySymmetricGraph wordEmbedding resolveRedirect=
    let nodeSet :: HS.HashSet PageId
        nodeSet = expandNodesK binarySymmetricGraph seeds radius

        nodeAttributes = nodesToAttributes annsFile wordEmbedding nodeSet

        universeSubset ::  HM.HashMap PageId [EdgeDoc]
--         universeSubset = trace (" empty in nodeSet " ++ show ("" `HS.member` nodeSet)) $ subsetOfUniverseGraph universeGraph nodeSet
        universeSubset = subsetOfUniverseGraph universeGraph nodeSet

        fixRedirectEdgeDocs :: EdgeDoc -> EdgeDoc
        fixRedirectEdgeDocs edgeDoc@EdgeDoc{..} =
            edgeDoc { edgeDocArticleId = resolveRedirect edgeDocArticleId
                    , edgeDocNeighbors = fmap resolveRedirect edgeDocNeighbors}

        edgeDocsSubset :: [EdgeDoc]
        edgeDocsSubset = HS.toList $ HS.fromList $ fmap fixRedirectEdgeDocs $ concat $ HM.elems universeSubset

        edgeFilters :: [(EdgeFilteringNames, [EdgeDoc] -> [EdgeDoc])]
        edgeFilters = [(BidiFiltered,  onlySymmetricEdges)
                      ,(Unfiltered,    id)
                      ]

        irModels :: [(RetrievalFun, Index.RetrievalModel Term EdgeDoc Int)]
        irModels = [ (Ql,  QL.queryLikelihood $ QL.Dirichlet 100 )
                   , (Bm25, BM25.bm25 $ BM25.sensibleParams )
                   ]


        isNotFromQueryPage :: EdgeDoc -> Bool
        isNotFromQueryPage edgeDoc@EdgeDoc{..} =
            edgeDocArticleId /= queryPageId

        irRankings :: [(RetrievalFun, RetrievalResult EdgeDoc)]
        irRankings = [ (irname, fmap (first fixRedirectEdgeDocs) $ filter (isNotFromQueryPage . fst) $ retrieveDocs retrievalFun query)
                     | (irname, retrievalFun) <- irModels
                     ]

        addSeedNodes ::   HS.HashSet PageId ->  HM.HashMap PageId [EdgeDocWithScores] -> HM.HashMap PageId [EdgeDocWithScores]
        addSeedNodes seeds graph =
              let seedNodes ::  HM.HashMap PageId [EdgeDocWithScores]
                  seedNodes = HM.fromList [(seed, []) | seed <- HS.toList seeds]
              in HM.unionWith (++) graph seedNodes
              
        fancyGraphs :: [(GraphNames, RetrievalFun, HM.HashMap PageId [EdgeDocWithScores])]
        fancyGraphs = concat [--(Top5PerNode,     const $ filterGraphByTop5NodeEdges  retrieveDocs      query)
                       [(Top100PerGraph, irname, addSeedNodes seeds $ filterGraphByTopNGraphEdges retrievalResult 100)
                      ,(Top10PerGraph, irname,   addSeedNodes seeds $ filterGraphByTopNGraphEdges retrievalResult 10)
                      ,(Top50PerGraph, irname,   addSeedNodes seeds $ filterGraphByTopNGraphEdges retrievalResult 50)
                      ,(Top200PerGraph, irname,  addSeedNodes seeds $ filterGraphByTopNGraphEdges retrievalResult 200)
                      ,(Top2000PerGraph, irname, addSeedNodes seeds $ filterGraphByTopNGraphEdges retrievalResult 2000)
                      ,(Top20000PerGraph, irname, addSeedNodes seeds $ filterGraphByTopNGraphEdges retrievalResult 20000)
                      ]
                      | (irname, retrievalResult) <- irRankings]

        simpleGraphs :: [(GraphNames, [EdgeDoc] -> HM.HashMap PageId (HM.HashMap PageId [EdgeDoc]))]
        simpleGraphs =  [(SimpleGraph, noFilterTwice )
                        ,(RandomGraph, randomFilter 100)
                        ,(Random2000Graph, randomFilter 2000)
                        ]

        weightings :: [(WeightingNames, EdgeDocWithScores -> Double)]
        weightings =  [ (Count, realToFrac . withScoreCount)
                      , (Score, realToFrac . withScoreScore)
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
                        ,(PersPageRank, \graph -> let wgraph = toGraph graph
                                                  in rankByPersonalizedPageRank wgraph 0.15 seeds 20)
                        ,(AttriRank, \graph ->  let wgraph = toGraph graph
                                                    embeddingBounds = wordEmbeddingDimBounds wordEmbedding
                                                in rankByAttriPageRank wgraph 0.15 embeddingBounds nodeAttributes 20)
                        ,(ShortPath, \graph -> let wgraph = toGraph graph
                                               in rankByShortestPaths (fmap (max $ Sum 0.001) $ coerce wgraph) (toList seeds))
                        ,(MargEdges, \graph -> marginalizeEdges graph)
                        ]

        fancyWeightedGraphs ::  [((GraphNames, EdgeFilteringNames, WeightingNames, RetrievalFun), HM.HashMap PageId (HM.HashMap PageId Double))]
        fancyWeightedGraphs =  [((gname, Unfiltered, wname, irname), accumulateEdgeWeights graph weighting seeds)
                               | (gname, irname, graph) <- fancyGraphs
                               , (wname, weighting) <- weightings
                               ]

        simpleWeightedGraphs :: [((GraphNames, EdgeFilteringNames, WeightingNames, RetrievalFun), HM.HashMap PageId (HM.HashMap PageId Double))]
        simpleWeightedGraphs = concat [ [ ((gname, ename, Count, NoIr),  fmap (fmap (realToFrac . length)) graph)
                                        , ((gname, ename, Binary, NoIr), fmap (fmap (const 1))             graph)
                                        ]
                                      | (ename, edgeFilter) <- edgeFilters
                                      , (gname, mkGraph) <- simpleGraphs
                                      , let graph = mkGraph $ edgeFilter edgeDocsSubset
                                      ]

        computeRankings' :: [(Method,  [(PageId, Double)])]
        computeRankings' =
            [ (Method gname ename wname rname irname,  graphRanking graph )
            | ((gname, ename, wname, irname), graph) <- simpleWeightedGraphs ++ fancyWeightedGraphs,
              (rname, graphRanking) <- graphRankings
            ] ++ [(CandidateSet, candidateSetList seeds radius binarySymmetricGraph)]

--     in (fancyGraphs, simpleGraphs) `deepseq` computeRankings'
    in computeRankings'




computeSimpleGraphs :: forall n. (KnownNat n)
                        => AnnotationsFile
                        -> HS.HashSet PageId -> HS.HashSet PageId -> Int -> UniverseGraph -> BinarySymmetricGraph
                        -> WordEmbedding n
                        -> (PageId -> PageId)
                        -> [(GraphNames, WeightingNames, HM.HashMap PageId (HM.HashMap PageId [EdgeDoc]))]

computeSimpleGraphs annsFile queryPageIds seeds radius universeGraph binarySymmetricGraph wordEmbedding resolveRedirect=
    let nodeSet :: HS.HashSet PageId
        nodeSet = expandNodesK binarySymmetricGraph seeds radius

        universeSubset ::  HM.HashMap PageId [EdgeDoc]
        universeSubset = subsetOfUniverseGraph universeGraph nodeSet

        fixRedirectEdgeDocs :: EdgeDoc -> EdgeDoc
        fixRedirectEdgeDocs edgeDoc@EdgeDoc{..} =
            edgeDoc { edgeDocArticleId = resolveRedirect edgeDocArticleId
                    , edgeDocNeighbors = fmap resolveRedirect edgeDocNeighbors}

        edgeDocsSubset :: [EdgeDoc]
        edgeDocsSubset =
            HS.toList $ HS.fromList
          $ filter isNotFromQueryPage
          $ fmap fixRedirectEdgeDocs
          $ concat $ HM.elems universeSubset
          where
            isNotFromQueryPage :: EdgeDoc -> Bool
            isNotFromQueryPage EdgeDoc{..} =
                not $ edgeDocArticleId `HS.member` queryPageIds


        edgeFilters :: [(EdgeFilteringNames, [EdgeDoc] -> [EdgeDoc])]
        edgeFilters = [ (BidiFiltered,  onlySymmetricEdges)
                      , (Unfiltered,    id)
                      ]

        weightings :: [(WeightingNames, HM.HashMap PageId (HM.HashMap PageId [EdgeDoc]) -> HM.HashMap PageId (HM.HashMap PageId Double))]
        weightings =
            [ (Count,  fmap (fmap (realToFrac . length)))
            , (Binary, fmap (fmap (const 1)))
            ]

        graphs = [(SimpleGraph,     noFilterTwice)
                 ,(RandomGraph,     randomFilter 100)
                 ,(Random2000Graph, randomFilter 2000)
                 ]
     in [ (graphName, wName, graph)
        | (graphName, mkGraph) <- graphs
        , (wName, weighting) <- weightings
        , (eName, edgeFilter) <- edgeFilters
        , let graph = mkGraph $ edgeFilter edgeDocsSubset
        ]

-- ^^^ not query dependent


computeFullgraphRankingsForQuery :: forall n. (KnownNat n)
                        => AnnotationsFile
                        -> [((GraphNames, EdgeFilteringNames, WeightingNames), HM.HashMap PageId (HM.HashMap PageId Double))]
                        -> HS.HashSet PageId -> Int -> UniverseGraph -> BinarySymmetricGraph
                        -> WordEmbedding n
                        -> (PageId -> PageId)
                        -> [(Method, [(PageId, Double)])]

computeFullgraphRankingsForQuery annsFile simpleWeightedGraphs seeds radius universeGraph binarySymmetricGraph wordEmbedding resolveRedirect=

-- vvv query dependent --
    let nodeSet :: HS.HashSet PageId
        nodeSet = expandNodesK binarySymmetricGraph seeds radius
        nodeAttributes = nodesToAttributes annsFile wordEmbedding nodeSet

        graphRankings :: [(GraphRankingNames, (HM.HashMap PageId (HM.HashMap PageId Double) -> [(PageId, Double)]))]
        graphRankings = [(PageRank, \graph ->  let wgraph = toGraph graph
                                               in rankByPageRank wgraph 0.15 20)
                        ,(PersPageRank, \graph -> let wgraph = toGraph graph
                                                  in rankByPersonalizedPageRank wgraph 0.15 seeds 20)
                        ,(AttriRank, \graph ->  let wgraph = toGraph graph
                                                    embeddingBounds = wordEmbeddingDimBounds wordEmbedding
                                                in rankByAttriPageRank wgraph 0.15 embeddingBounds nodeAttributes 20)
                        ,(ShortPath, \graph -> let wgraph = toGraph graph
                                               in rankByShortestPaths (fmap (max $ Sum 0.001) $ coerce wgraph) (toList seeds))
                        ,(MargEdges, \graph -> marginalizeEdges graph)
                        ]

        computeRankings' :: [(Method,  [(PageId, Double)])]
        computeRankings' =
            [ (Method gname ename wname rname NoIr,  graphRanking graph )
            | ((gname, ename, wname), graph) <- simpleWeightedGraphs ,
              (rname, graphRanking) <- graphRankings
            ] ++ [(CandidateSet, candidateSetList seeds radius binarySymmetricGraph)]

--     in (fancyGraphs, simpleGraphs) `deepseq` computeRankings'
    in computeRankings'




dotGraph :: HM.HashMap PageId (HM.HashMap PageId Double) -> Dot.DotGraph PageId
dotGraph graph = Dot.graphElemsToDot params nodes edges
  where
    params = Dot.nonClusteredParams { Dot.fmtEdge = \(_,_,w) -> [ Dot.penWidth (w/10.0), Dot.Weight $ Dot.Int (ceiling w) ]
                                    , Dot.fmtNode = \(_,a) -> [Dot.toLabel a]
                                    , Dot.globalAttributes = [ Dot.GraphAttrs [ Dot.OutputOrder Dot.EdgesFirst
                                                                              , Dot.Overlap $ Dot.PrismOverlap Nothing] ]
                                    }
    nodes = [ (a,a) | a <- HM.keys graph ]
    edges = [ (a,b,w)
            | (a, ns) <- HM.toList graph
            , (b, w) <- HM.toList ns
            ]

computeGraphForQuery ::(Index.RetrievalModel Term EdgeDoc Int -> RetrievalFunction EdgeDoc)
                     -> AnnotationsFile
                     -> [Term]
                     -> HS.HashSet PageId
                     -> FilePath
                     -> IO ()
computeGraphForQuery retrieveDocs annsFile query seeds dotFilename = do
    let
        irModel = BM25.bm25 @Term $ BM25.sensibleParams
        retrievalResult = (retrieveDocs irModel) query
        fancyGraph =  filterGraphByTopNGraphEdges retrievalResult 50

        weighting :: EdgeDocWithScores -> Double
        weighting = realToFrac . withScoreScore

        fancyWeightedGraph ::  HM.HashMap PageId (HM.HashMap PageId Double)
        fancyWeightedGraph =  accumulateEdgeWeights fancyGraph weighting seeds

        graph = dotGraph fancyWeightedGraph  --todo highlight seeds
    Dot.writeDotFile (dotFilename ++ ".dot") graph
    void $ Dot.runGraphvizCommand Dot.Neato graph Dot.Svg dotFilename

instance Dot.PrintDot PageId where
    unqtDot x = Dot.unqtText $ TL.pack $ unpackPageId x
    toDot x = Dot.printEscaped [] $ TL.pack $ unpackPageId x

instance Dot.Labellable PageId where
    toLabelValue x = Dot.StrLabel $ TL.pack $ unpackPageId x

dummyInvalidPageId :: PageId
dummyInvalidPageId = packPageId "__invalid__"



logMsg :: PageId -> Method -> String -> IO ()
logMsg queryId method t = T.putStr $ T.pack $ unpackPageId queryId++"\t"++showMethodName method++"\t"++t++"\n"


timeIt :: IO a -> IO (Double, a)
timeIt action = do
      t0 <- getCurrentTime
      !r <- action
      t1 <- getCurrentTime
      let dt = t1 `diffUTCTime` t0
      return (realToFrac dt / 60 :: Double, r)

logTimed :: PageId -> Method -> String -> IO a -> IO a
logTimed queryId method msg doIt = do
      logMsg queryId method msg
      (t, r) <- timeIt doIt
      logMsg queryId method $ msg++"\ttime="++(showFFloat (Just 3) t "")
      return r



main :: IO ()
main = do
    (articlesFile, outputFilePrefix, embeddingsFile, querySrc, runMethods, expansionHops, simplirIndexFilepath,
      graphset, queryMaybe, dotFilenameMaybe) <-
        execParser $ info (helper <*> opts) mempty
    annsFile <- AnnsFile.openAnnotations articlesFile
    putStrLn $ "# Running methods: " ++ show runMethods
    putStrLn $ "# Query restriction " ++ show queryMaybe
    putStrLn $ "# Edgedoc index "++ show simplirIndexFilepath

    SomeWordEmbedding wordEmbeddings <- readGlove embeddingsFile -- "/home/dietz/trec-car/code/lstm-car/data/glove.6B.50d.txt"

    let resolveRedirect = resolveRedirectFactory $ AnnsFile.pages annsFile

            
    let universeGraph :: UniverseGraph
        universeGraph = edgeDocsToUniverseGraph $ pagesToEdgeDocs $ AnnsFile.pages annsFile

    let binarySymmetricGraph :: BinarySymmetricGraph
        binarySymmetricGraph = universeToBinaryGraph universeGraph

    putStrLn ("nodes in KB = " <> show (HM.size universeGraph))

    queriesWithSeedEntities' <-
        case querySrc of
          QueriesFromCbor queryFile -> (pagesToLeadEntities resolveRedirect) . decodeCborList <$> BSL.readFile queryFile
          QueriesFromJson queryFile -> do
              QueryDocList queriesWithSeedEntities <- either error id . Data.Aeson.eitherDecode <$> BSL.readFile queryFile
              return queriesWithSeedEntities

    let queriesWithSeedEntities
          | Just query <- queryMaybe =  (trace $ "using only query "<>(unpackPageId query))
                                       $ filter (\q-> queryDocQueryId q == query ) queriesWithSeedEntities'
          | otherwise = queriesWithSeedEntities'

    index <- Index.open simplirIndexFilepath
    let retrieveDocs :: Index.RetrievalModel Term EdgeDoc Int -> RetrievalFunction EdgeDoc
        retrieveDocs model = map swap . Index.score index model

    handles <- sequence $ M.fromList  -- todo if we run different models in parallel, this will overwrite previous results.
      [ (method, openFile (outputFilePrefix ++ showMethodName method ++ ".run") WriteMode >>= newMVar)
      | method <- fromMaybe allMethods runMethods ]
        :: IO (M.Map Method (MVar Handle))

    ncaps <- getNumCapabilities



    let --forM_' = forM_
        forM_' = forConcurrentlyN_ ncaps

    let filterOutSeeds :: QueryDoc -> [(PageId, Double)] -> [(PageId, Double)]
        filterOutSeeds query ranking = filter notSeedEntity ranking      --  remove seed entities from ranking
            where notSeedEntity (entityId, _) =
                    (not $ entityId `HS.member` queryDocLeadEntities query)
                    && (not $ entityId == queryDocQueryId query)

        runMethod :: PageId -> QueryDoc -> Method -> [(PageId, Double)] -> IO ()
        runMethod queryId query method ranking = do
            let Just hdl = M.lookup method handles

            logMsg queryId method $ "evaluating"
            --logTimed "evaluating graph" $ evaluate $ rnf graph
            --logMsg $ "graph size: "++show (graphSize graph)
            --ranking <- logTimed "computing ranking" $ evaluate $ force $ computeRanking graph
            logMsg queryId method $ "ranking entries="++show (length ranking)
            ranking' <- if null ranking                        -- replace empty rankings with dummy result (for trec_eval)
                           then do logMsg queryId method $ "empty result set replaced by dummy result"
                                   pure [(dummyInvalidPageId, 0.0)]
                           else pure ranking

            let ranking'' = filterOutSeeds query ranking'

                formatted = WriteRanking.formatEntityRankings
                            (T.pack $ show method)
                            (T.pack $ unpackPageId queryId)
                            ranking''
            bracket (takeMVar hdl) (putMVar hdl) $ \ h ->
                TL.hPutStrLn h formatted

    let fullgraphExpansion = do

            forM_' queriesWithSeedEntities $ \query@QueryDoc{queryDocQueryId=queryId, queryDocLeadEntities=seedEntities} -> do
                when (null $ seedEntities) $
                    T.putStr $ T.pack $ "# Query with no lead entities: "++show query++"\n"

                T.putStr $ T.pack $ "# Processing query "++ show query++"\n"
                let simpleWeightedGraphs = undefined computeSimpleGraphs

                    rankings :: [(Method, [(PageId, Double)])]
                    rankings = computeFullgraphRankingsForQuery annsFile simpleWeightedGraphs queryId seedEntities expansionHops
                                          universeGraph binarySymmetricGraph wordEmbeddings resolveRedirect

                let methodsAvailable = S.fromList (map fst rankings)
                    badMethods
                      | Just ms <- runMethods = S.fromList ms `S.difference` methodsAvailable
                      | otherwise             = S.empty
                    filterMethods
                      | Just ms <- runMethods = (`elem` ms)
                      | otherwise             = const True
                when (not $ S.null badMethods) $ putStrLn $ "\n\nwarning: unknown methods: "++show badMethods++"\n"
                mapM_ (uncurry $ runMethod queryId query) $ filter (filterMethods . fst) rankings


    let subgraphExpansion = do
            forM_' queriesWithSeedEntities $ \query@QueryDoc{queryDocQueryId=queryId, queryDocLeadEntities=seedEntities} -> do
                when (null $ seedEntities) $
                    T.putStr $ T.pack $ "# Query with no lead entities: "++show query++"\n"

                T.putStr $ T.pack $ "# Processing query "++ show query++"\n"
                let rankings :: [(Method, [(PageId, Double)])]
                    rankings = computeRankingsForQuery retrieveDocs annsFile queryId (queryDocRawTerms query) seedEntities expansionHops
                                          universeGraph binarySymmetricGraph wordEmbeddings resolveRedirect

                case dotFilenameMaybe of
                    Just dotFilename -> computeGraphForQuery retrieveDocs  annsFile (queryDocRawTerms query) seedEntities  dotFilename
                    Nothing -> return ()

                let methodsAvailable = S.fromList (map fst rankings)
                    badMethods
                      | Just ms <- runMethods = S.fromList ms `S.difference` methodsAvailable
                      | otherwise             = S.empty
                    filterMethods
                      | Just ms <- runMethods = (`elem` ms)
                      | otherwise             = const True
                when (not $ S.null badMethods) $ putStrLn $ "\n\nwarning: unknown methods: "++show badMethods++"\n"
                mapM_ (uncurry $ runMethod queryId query) $ filter (filterMethods . fst) rankings

    case graphset of
      Fullgraph -> fullgraphExpansion
      Subgraph ->  subgraphExpansion

    mapM_ (\h -> takeMVar h >>= hClose) handles


forConcurrentlyN_ :: Foldable f => Int -> f a -> (a -> IO ()) -> IO ()
forConcurrentlyN_ n xs f = do
    sem <- atomically $ newTSem n
    let run x = bracket (atomically $ waitTSem sem) (const $ atomically $ signalTSem sem) (const $ f x)
    forConcurrently_ xs run
