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
import Data.List (sortBy)
import Data.Maybe
import Data.Ord
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

import Graph
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

data QuerySource = QueriesFromCbor FilePath
                 | QueriesFromCborAndEntityIndex FilePath (Index.OnDiskIndex Term PageId Int)
                 | QueriesFromJson FilePath


data Graphset = Fullgraph | Subgraph

opts :: Parser ( FilePath, FilePath, FilePath, QuerySource
               , Maybe [Method], Int, Index.OnDiskIndex Term EdgeDoc Int
               , Graphset
               , [PageId], Maybe FilePath)
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
    <*> flag Subgraph Fullgraph (long "fullgraph" <> help "Run on full graph, not use subgraph retrieval")
    <*> many (option (packPageId <$> str) (long "query" <> metavar "QUERY" <> help "execute only this query"))
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
          <|> fromEntityIndex
        where
          fromEntityIndex =
              QueriesFromCborAndEntityIndex
                <$> option str (short 'Q' <> long "queries-nolead" <> metavar "CBOR" <> help "Queries from CBOR pages taking seed entities from entity retrieval")
                <*> option (Index.OnDiskIndex <$> str) (long "entity-index" <> metavar "INDEX" <> help "Entity index path")


candidateSetList :: HS.HashSet PageId -> Int -> BinarySymmetricGraph
                ->  [(PageId, Double)]
candidateSetList seeds radius binarySymmetricGraph  =
    let nodes :: HS.HashSet PageId
        nodes = expandNodesK binarySymmetricGraph seeds radius
   in [ (pageId, 1.0) | pageId <- HS.toList nodes]



nodesToAttributes :: forall n. (KnownNat n)
                 => AnnotationsFile
                 -> WordEmbedding n
                 -> HS.HashSet PageId
                 -> HM.HashMap PageId (Attributes (EmbeddingDim n))
nodesToAttributes annsFile wordEmbedding nodes =
    zScoreStandardize
    $ HM.mapWithKey (\pid _ -> toWordVec pid)
    $ HS.toMap nodes
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
    let nodes :: HS.HashSet PageId
        nodes = expandNodesK binarySymmetricGraph seeds radius

        nodeAttributes = nodesToAttributes annsFile wordEmbedding nodes

        universeSubset ::  HM.HashMap PageId [EdgeDoc]
--         universeSubset = trace (" empty in nodeSet " ++ show ("" `HS.member` nodeSet)) $ subsetOfUniverseGraph universeGraph nodeSet
        universeSubset = subsetOfUniverseGraph universeGraph nodes

        fixRedirectEdgeDocs :: EdgeDoc -> EdgeDoc
        fixRedirectEdgeDocs edgeDoc@EdgeDoc{..} =
            edgeDoc { edgeDocArticleId = resolveRedirect edgeDocArticleId
                    , edgeDocNeighbors = HS.map resolveRedirect edgeDocNeighbors
                    }

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
        isNotFromQueryPage EdgeDoc{..} =
            edgeDocArticleId /= queryPageId

        irRankings :: [(RetrievalFun, RetrievalResult EdgeDoc)]
        irRankings = [ (irname, fmap (first fixRedirectEdgeDocs) $ filter (isNotFromQueryPage . fst) $ retrieveDocs retrievalFun query)
                     | (irname, retrievalFun) <- irModels
                     ]

        addSeedNodes :: HS.HashSet PageId -> HM.HashMap PageId [EdgeDocWithScores] -> HM.HashMap PageId [EdgeDocWithScores]
        addSeedNodes seeds graph =
              let seedNodes ::  HM.HashMap PageId [EdgeDocWithScores]
                  seedNodes = HM.fromList [(seed, []) | seed <- HS.toList seeds]
              in HM.unionWith (++) graph seedNodes

        fancyGraphs :: [(GraphNames, RetrievalFun, HM.HashMap PageId [EdgeDocWithScores])]
        fancyGraphs = concat [--(Top5PerNode,     const $ filterGraphByTop5NodeEdges  retrieveDocs      query)
                               [ (Top100PerGraph,   irname, addSeedNodes seeds $ filterGraphByTopNGraphEdges retrievalResult 100)
                               , (Top10PerGraph,    irname, addSeedNodes seeds $ filterGraphByTopNGraphEdges retrievalResult 10)
                               , (Top50PerGraph,    irname, addSeedNodes seeds $ filterGraphByTopNGraphEdges retrievalResult 50)
                               , (Top200PerGraph,   irname, addSeedNodes seeds $ filterGraphByTopNGraphEdges retrievalResult 200)
                               , (Top2000PerGraph,  irname, addSeedNodes seeds $ filterGraphByTopNGraphEdges retrievalResult 2000)
                               , (Top20000PerGraph, irname, addSeedNodes seeds $ filterGraphByTopNGraphEdges retrievalResult 20000)
                               ]
                             | (irname, retrievalResult) <- irRankings
                             ]

        simpleGraphs :: [(GraphNames, [EdgeDoc] -> Graph PageId [EdgeDoc])]
        simpleGraphs =  [(SimpleGraph, noFilterTwice)
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


        graphRankings :: [(GraphRankingNames, Graph PageId Double -> [(PageId, Double)])]
        graphRankings = [(PageRank, \graph -> rankByPageRank graph 0.15 20)
                        ,(PersPageRank, \graph -> rankByPersonalizedPageRank graph 0.15 seeds 20)
                        ,(AttriRank, \graph ->  let embeddingBounds = wordEmbeddingDimBounds wordEmbedding
                                                in rankByAttriPageRank graph 0.15 embeddingBounds nodeAttributes 20)
                        ,(ShortPath, \graph -> rankByShortestPaths (fmap (max $ Sum 0.001) $ coerce graph) (toList seeds))
                        ,(MargEdges, \graph -> marginalizeEdges graph)
                        ]

        fancyWeightedGraphs ::  [((GraphNames, EdgeFilteringNames, WeightingNames, RetrievalFun), Graph PageId Double)]
        fancyWeightedGraphs =  [((gname, Unfiltered, wname, irname), accumulateEdgeWeights graph weighting seeds)
                               | (gname, irname, graph) <- fancyGraphs
                               , (wname, weighting) <- weightings
                               ]

        simpleWeightedGraphs :: [((GraphNames, EdgeFilteringNames, WeightingNames, RetrievalFun), Graph PageId Double)]
        simpleWeightedGraphs = [ ((gname, ename, wname, NoIr), graph)
                               | (ename, edgeFilter) <- edgeFilters
                               , (gname, mkGraph) <- simpleGraphs
                               , (wname, weighting) <- [ (Count, fmap (realToFrac . length))
                                                       , (Binary, fmap (const 1))
                                                       ]
                               , let graph = weighting $ mkGraph $ edgeFilter edgeDocsSubset
                               ]

        computeRankings' :: [(Method,  [(PageId, Double)])]
        computeRankings' =
            [ (Method gname ename wname rname irname,  graphRanking graph )
            | ((gname, ename, wname, irname), graph) <- simpleWeightedGraphs ++ fancyWeightedGraphs,
              (rname, graphRanking) <- graphRankings
            ] ++ [(CandidateSet, candidateSetList seeds radius binarySymmetricGraph)]

--     in (fancyGraphs, simpleGraphs) `deepseq` computeRankings'
    in computeRankings'




computeSimpleGraphs :: UniverseGraph
                    -> (PageId -> PageId)
                    -> HS.HashSet PageId
                    -> [((GraphNames, EdgeFilteringNames, WeightingNames), Graph PageId Double)]

computeSimpleGraphs universeGraph resolveRedirect queryPageIds =
    let universeSubset ::  HM.HashMap PageId [EdgeDoc]
        universeSubset = universeGraph

        fixRedirectEdgeDocs :: EdgeDoc -> EdgeDoc
        fixRedirectEdgeDocs edgeDoc@EdgeDoc{..} =
            edgeDoc { edgeDocArticleId = resolveRedirect edgeDocArticleId
                    , edgeDocNeighbors = HS.map resolveRedirect edgeDocNeighbors
                    }

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

        weightings :: [(WeightingNames, Graph PageId [EdgeDoc] -> Graph PageId Double)]
        weightings =
            [ (Count,  fmap (realToFrac . length))
            , (Binary, fmap (const 1))
            ]

        graphs :: [(GraphNames, [EdgeDoc] -> Graph PageId [EdgeDoc])]
        graphs = [(SimpleGraph,     noFilterTwice)
                 ,(RandomGraph,     randomFilter 100)
                 ,(Random2000Graph, randomFilter 2000)
                 ]
     in [ ((graphName, eName, wName), graph)
        | (graphName, mkGraph) <- graphs
        , (wName, weighting) <- weightings
        , (eName, edgeFilter) <- edgeFilters
        , let graph = weighting $ mkGraph $ edgeFilter edgeDocsSubset
        ]

-- ^^^ not query dependent

-- vvv query dependent --

computeFullgraphRankingsForQuery
    :: forall n. (KnownNat n)
    => AnnotationsFile
    -> WordEmbedding n
    -> [((GraphNames, EdgeFilteringNames, WeightingNames), Graph PageId Double)]
    -> HS.HashSet PageId
    -> [(Method, [(PageId, Double)])]

computeFullgraphRankingsForQuery annsFile wordEmbedding simpleWeightedGraphs = \seeds ->
    let graphRankings :: [(GraphRankingNames, Graph PageId Double -> [(PageId, Double)])]
        graphRankings = [(PageRank, \graph -> rankByPageRank graph 0.15 20)
                        ,(PersPageRank, \graph -> rankByPersonalizedPageRank graph 0.15 seeds 20)
                        ,(AttriRank, \graph ->  let embeddingBounds = wordEmbeddingDimBounds wordEmbedding
                                                in rankByAttriPageRank graph 0.15 embeddingBounds nodeAttributes 20)
                        ,(ShortPath, \graph -> rankByShortestPaths (fmap (max $ Sum 0.001) $ coerce graph) (toList seeds))
                        ,(MargEdges, \graph -> marginalizeEdges graph)
                        ]

        computeRankings' :: [(Method,  [(PageId, Double)])]
        computeRankings' =
            [ (Method gname ename wname rname NoIr,  graphRanking graph )
            | ((gname, ename, wname), graph) <- simpleWeightedGraphs ,
              (rname, graphRanking) <- graphRankings
            ]
    in computeRankings'
  where
    nodeAttributes = nodesToAttributes annsFile wordEmbedding (foldMap (nodeSet . snd) simpleWeightedGraphs)



dotGraph :: Graph PageId Double -> Dot.DotGraph PageId
dotGraph graph = Dot.graphElemsToDot params nodes edges
  where
    params = Dot.nonClusteredParams { Dot.fmtEdge = \(_,_,w) -> [ Dot.penWidth (w/10.0), Dot.Weight $ Dot.Int (ceiling w) ]
                                    , Dot.fmtNode = \(_,a) -> [Dot.toLabel a]
                                    , Dot.globalAttributes = [ Dot.GraphAttrs [ Dot.OutputOrder Dot.EdgesFirst
                                                                              , Dot.Overlap $ Dot.PrismOverlap Nothing] ]
                                    }
    nodes = [ (a,a) | a <- HS.toList $ nodeSet graph ]
    edges = [ (a,b,w)
            | (a, ns) <- HM.toList $ getGraph graph
            , (b, w) <- HM.toList ns
            ]

computeGraphForQuery ::(Index.RetrievalModel Term EdgeDoc Int -> RetrievalFunction EdgeDoc)
                     -> [Term]
                     -> HS.HashSet PageId
                     -> FilePath
                     -> IO ()
computeGraphForQuery retrieveDocs query seeds dotFilename = do
    let
        irModel = BM25.bm25 @Term $ BM25.sensibleParams
        retrievalResult = (retrieveDocs irModel) query
        fancyGraph =  filterGraphByTopNGraphEdges retrievalResult 50

        weighting :: EdgeDocWithScores -> Double
        weighting = realToFrac . withScoreScore

        fancyWeightedGraph :: Graph PageId Double
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
    hSetBuffering stdout LineBuffering

    (articlesFile, outputFilePrefix, embeddingsFile, querySrc, runMethods, expansionHops, simplirIndexFilepath,
      graphset, queryRestriction, dotFilenameMaybe) <-
        execParser $ info (helper <*> opts) mempty
    putStrLn $ "# Pages: " ++ show articlesFile
    annsFile <- AnnsFile.openAnnotations articlesFile
    putStrLn $ "# Running methods: " ++ show runMethods
    putStrLn $ "# Query restriction: " ++ show queryRestriction
    putStrLn $ "# Edgedoc index: "++ show simplirIndexFilepath

    SomeWordEmbedding wordEmbeddings <- readGlove embeddingsFile
    putStrLn $ "# Embedding: " ++ show embeddingsFile ++ ", dimension=" ++ show (wordEmbeddingDim wordEmbeddings)

    let !resolveRedirect = resolveRedirectFactory $ AnnsFile.pages annsFile
    putStrLn $ "# computed redirects"

    let universeGraph :: UniverseGraph
        !universeGraph = edgeDocsToUniverseGraph $ pagesToEdgeDocs $ AnnsFile.pages annsFile
    putStrLn $ "# nodes in KB = " <> show (HM.size universeGraph)

    let binarySymmetricGraph :: BinarySymmetricGraph
        !binarySymmetricGraph = universeToBinaryGraph universeGraph
    putStrLn $ "# symmetric graph size = " <> show (HM.size binarySymmetricGraph)


    queriesWithSeedEntities' <-
        case querySrc of
          QueriesFromCbor queryFile -> pagesToLeadEntities resolveRedirect . decodeCborList <$> BSL.readFile queryFile
          QueriesFromCborAndEntityIndex queryFile entityIndexFile -> do
              entityIndex <- Index.open entityIndexFile
              putStrLn $ "# entity index: " ++ show entityIndexFile
              let entitiesFromIndex :: QueryDoc -> QueryDoc
                  entitiesFromIndex qdoc =
                      qdoc { queryDocLeadEntities = seeds }
                    where
                      queryTerms = textToTokens' $ queryDocQueryText qdoc
                      seedModel = QL.queryLikelihood (QL.Dirichlet 100)
                      seeds = HS.fromList $ map snd
                              $ take 5 $ sortBy (flip $ comparing snd)
                              $ Index.score entityIndex seedModel queryTerms

              map entitiesFromIndex . pagesToLeadEntities resolveRedirect . decodeCborList <$> BSL.readFile queryFile
          QueriesFromJson queryFile -> do
              QueryDocList queriesWithSeedEntities <- either error id . Data.Aeson.eitherDecode <$> BSL.readFile queryFile
              return queriesWithSeedEntities

    queriesWithSeedEntities <-
        if null queryRestriction
          then return queriesWithSeedEntities'
          else do putStrLn $ "# using only queries "<>show queryRestriction
                  return $ filter (\q-> queryDocQueryId q `elem` queryRestriction) queriesWithSeedEntities'
    putStrLn $ "# query count: " ++ show (length queriesWithSeedEntities)

    index <- Index.open simplirIndexFilepath
    let retrieveDocs :: Index.RetrievalModel Term EdgeDoc Int -> RetrievalFunction EdgeDoc
        retrieveDocs model = map swap . Index.score index model
    putStrLn "# opened edgedoc index"

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
            putStrLn "full graph"
            forM_' queriesWithSeedEntities $ \query@QueryDoc{queryDocQueryId=queryId, queryDocLeadEntities=seedEntities} -> do
                when (null $ seedEntities) $
                    T.putStr $ T.pack $ "# Query with no lead entities: "++show query++"\n"

                T.putStr $ T.pack $ "# Processing query "++ show query++": seeds=" ++ show seedEntities ++ "\n"

                let rankings :: [(Method, [(PageId, Double)])]
                    rankings = computeFullgraphRankingsForQuery
                                   annsFile wordEmbeddings simpleWeightedGraphs
                                   seedEntities

                let methodsAvailable = S.fromList (map fst rankings)
                    badMethods
                      | Just ms <- runMethods = S.fromList ms `S.difference` methodsAvailable
                      | otherwise             = S.empty
                    filterMethods
                      | Just ms <- runMethods = (`elem` ms)
                      | otherwise             = const True
                when (not $ S.null badMethods) $ putStrLn $ "\n\nwarning: unknown methods: "++show badMethods++"\n"
                mapM_ (uncurry $ runMethod queryId query) $ filter (filterMethods . fst) rankings
          where
            simpleWeightedGraphs = computeSimpleGraphs universeGraph resolveRedirect queryPageIds
            queryPageIds = HS.fromList $ map queryDocQueryId queriesWithSeedEntities


    let subgraphExpansion = do
            forM_' queriesWithSeedEntities $ \query@QueryDoc{queryDocQueryId=queryId, queryDocLeadEntities=seedEntities} -> do
                when (null $ seedEntities) $
                    T.putStr $ T.pack $ "# Query with no lead entities: "++show query++"\n"

                T.putStr $ T.pack $ "# Processing query "++ show query++": seeds=" ++ show seedEntities ++ "\n"
                let rankings :: [(Method, [(PageId, Double)])]
                    rankings = computeRankingsForQuery retrieveDocs annsFile queryId (queryDocRawTerms query) seedEntities expansionHops
                                          universeGraph binarySymmetricGraph wordEmbeddings resolveRedirect

                case dotFilenameMaybe of
                    Just dotFilename -> computeGraphForQuery retrieveDocs (queryDocRawTerms query) seedEntities  dotFilename
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
