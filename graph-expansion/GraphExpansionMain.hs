{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad (when, void)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Control.Exception
import Data.List (sortBy)
import Data.Maybe
import Data.Tuple
import Data.Semigroup hiding (All, Any, option)
import Data.Foldable
import Data.Coerce
import Options.Applicative
import System.IO
import Data.Time.Clock
import Numeric
import GHC.TypeLits
import Data.Aeson
import Numeric.Log

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
import CAR.ToolVersion
import CAR.AnnotationsFile as CAR
import CAR.Retrieve as Retrieve
import qualified CAR.RunFile as CarRun

import Graph
import EdgeDocCorpus
import WriteRanking
import GraphExpansion
import GraphExpansionExperiments
import SimplIR.WordEmbedding
import SimplIR.WordEmbedding.Parse
import qualified SimplIR.SimpleIndex as Index
import qualified SimplIR.SimpleIndex.Models.BM25 as BM25
import SimplIR.TopK (collectTopK)
import Control.Concurrent.Map
import ZScore

type NumResults = Int

type EntityIndex = Index.OnDiskIndex Term PageId Int

data SeedDerivation = SeedsFromLeadSection
                    | SeedsFromEntityIndex EntityIndex
                    -- | SeedsFromHeadingEntityLinks -- TODO

data QuerySource = QueriesFromCbor FilePath QueryDerivation SeedDerivation
                 | QueriesFromJson FilePath

data Graphset = Fullgraph | Subgraph
data RankingType = EntityRanking | EntityPassageRanking
  deriving (Show)

opts :: Parser ( FilePath, FilePath, FilePath, QuerySource
               , Maybe [Method], Int, Index.OnDiskIndex Term EdgeDoc Int
               , RankingType
               , Graphset
               , [CarRun.QueryId]
               , Maybe FilePath
               , NumResults
               )
opts =
    (,,,,,,,,,,,)
    <$> argument str (help "articles file" <> metavar "ANNOTATIONS FILE")
    <*> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")
    <*> option str (short 'e' <> long "embedding" <> metavar "FILE" <> help "Glove embeddings file")
    <*> querySource
    <*> optional methods
    <*> option auto (long "hops" <> metavar "INT" <> help "number of hops for initial outward expansion" <> value 3)
    <*> option (Index.OnDiskIndex <$> str)
               (short 'i' <> long "index" <> metavar "INDEX" <> help "simplir edgedoc index")
    <*> flag EntityRanking EntityPassageRanking (long "entity-psg" <> help "If set, include provenance paragraphs")
    <*> flag Subgraph Fullgraph (long "fullgraph" <> help "Run on full graph, not use subgraph retrieval")
    <*> many (option (CarRun.QueryId . T.pack <$> str) (long "query" <> metavar "QUERY" <> help "execute only this query"))
    <*> optional (option str (long "dot" <> metavar "FILE" <> help "export dot graph to this file"))
    <*> option auto (short 'k' <> long "num-results" <> help "number of results per query")
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
      methodSet "trec" = pure trecMethods
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

      querySource :: Parser QuerySource
      querySource =
              fromCborTitle
          <|> option (fmap QueriesFromJson str) (short 'j' <> long "queries-json" <> metavar "JSON" <> help "Queries from JSON")
          <|> fromEntityIndex
        where
          queryDeriv =
              flag QueryFromPageTitle QueryFromSectionPaths
                   (long "query-from-sections" <> help "Use sections as query documents")
          fromCborTitle =
              QueriesFromCbor
                <$> option str (short 'q' <> long "queries" <> metavar "CBOR" <> help "Queries from CBOR pages")
                <*> queryDeriv
                <*> pure SeedsFromLeadSection

          fromEntityIndex =
              QueriesFromCbor
                <$> option str (short 'Q' <> long "queries-nolead" <> metavar "CBOR" <> help "Queries from CBOR pages taking seed entities from entity retrieval")
                <*> queryDeriv
                <*> option (SeedsFromEntityIndex . Index.OnDiskIndex <$> str) (long "entity-index" <> metavar "INDEX" <> help "Entity index path")




nodesToWordVector :: forall n. (KnownNat n)
                 => PageBundle
                 -> WordEmbedding n
                 -> HS.HashSet PageId
                 -> HM.HashMap PageId (Attributes (EmbeddingDim n))
nodesToWordVector pageBundle wordEmbedding nodes =
    zScoreStandardize
    $ HM.mapWithKey (\pid _ -> toWordVec pid)
    $ HS.toMap nodes
  where
    toWordVec pid =
        wordVecToAttributes
        $ maybe (pageNameEmbeddingAttributes wordEmbedding pid) (pageTextEmbeddingAttributes wordEmbedding)
        $ CAR.bundleLookupPage pageBundle pid

computeRankingsForQuery :: (Index.RetrievalModel Term EdgeDoc Int -> RetrievalFunction EdgeDoc)
                        -> PageBundle
                        -> CarRun.QueryId -> PageId ->  [Term] -> HS.HashSet PageId -> Int
                        -> [(Method, [(PageId, Maybe ParagraphId, Double)])]

computeRankingsForQuery
      retrieveDocs
      pageBundle queryId queryPageId query seeds radius =
      let

        edgeFilters :: [(EdgeFilteringNames, [EdgeDoc] -> [EdgeDoc])]
        edgeFilters = [(BidiFiltered,  onlySymmetricEdges)
                      ,(Unfiltered,    id)
                      ]

        isNotFromQueryPage :: EdgeDoc -> Bool
        isNotFromQueryPage EdgeDoc{..} =
            edgeDocArticleId /= queryPageId

        addSeedNodes :: HS.HashSet PageId -> HM.HashMap PageId [EdgeDocWithScores] -> HM.HashMap PageId [EdgeDocWithScores]
        addSeedNodes seeds graph =
              let seedNodes ::  HM.HashMap PageId [EdgeDocWithScores]
                  seedNodes = HM.fromList [(seed, []) | seed <- HS.toList seeds]
              in HM.unionWith (++) graph seedNodes


        retrievalResults :: [(RetrievalFun, [(EdgeDoc, Log Double)])]
        retrievalResults = [ (irname, retrievalResult)
                           | (irname, retrievalModel) <- retrievalModels
                           , let retrievalResult = filter (isNotFromQueryPage . fst)
                                                   $ retrieveDocs retrievalModel query
                           ]

        fancyGraphs :: [(GraphNames, RetrievalFun, HM.HashMap PageId [EdgeDocWithScores])]
        fancyGraphs = concat [ [ (Top100PerGraph,   irname, addSeedNodes seeds $ filterGraphByTopNGraphEdges retrievalResult 100)
                               , (Top10PerGraph,    irname, addSeedNodes seeds $ filterGraphByTopNGraphEdges retrievalResult 10)
                               , (Top50PerGraph,    irname, addSeedNodes seeds $ filterGraphByTopNGraphEdges retrievalResult 50)
                               , (Top200PerGraph,   irname, addSeedNodes seeds $ filterGraphByTopNGraphEdges retrievalResult 200)
                               , (Top2000PerGraph,  irname, addSeedNodes seeds $ filterGraphByTopNGraphEdges retrievalResult 2000)
                               , (Top20000PerGraph, irname, addSeedNodes seeds $ filterGraphByTopNGraphEdges retrievalResult 20000)
                               ]
                             | (irname, retrievalResult) <- retrievalResults
                             ]

        weightings :: [(WeightingNames, EdgeDocWithScores -> Double)]
        weightings =  [ (Count, realToFrac . withScoreCount)
                      , (Score, realToFrac . withScoreScore)
                      , (RecipRank,   (\edge ->  let w = 1.0 / (realToFrac $ withScoreRank edge )
                                                 in assert (w > 0) w
                        ))
                      , (LinearRank,  (\edge ->  let w = realToFrac (101 - (withScoreRank edge))
                                                 in assert (w > 0) w
                                      ))
                      , (BucketRank,  (\edge ->  let rank = withScoreRank $ edge
                                                     w = if rank <= 5 then 3.0 else
                                                            if rank <= 20 then 2.0 else
                                                            1.0
                                                 in assert (w > 0) w
                                      ))
                      ]


        graphRankings :: [(GraphRankingNames, Graph PageId Double -> [(PageId, Double)])]
        graphRankings = [(PageRank, \graph -> rankByPageRank graph 0.15 20)
                        ,(PersPageRank, \graph -> rankByPersonalizedPageRank graph 0.15 seeds 20)
                        ,(ShortPath, \graph -> rankByShortestPaths (fmap (max $ Sum 0.001) $ coerce graph) (toList seeds))
                        ,(MargEdges, \graph -> marginalizeEdges graph)
                        ]

        fancyWeightedGraphs ::  [((GraphNames, EdgeFilteringNames, WeightingNames, RetrievalFun), Graph PageId Double)]
        fancyWeightedGraphs =  [((gname, Unfiltered, wname, irname), accumulateEdgeWeights graph weighting seeds)
                               | (gname, irname, graph) <- fancyGraphs
                               , (wname, weighting) <- weightings
                               ]

        attachParagraphs :: RetrievalFun ->  [(PageId, Double)] -> [(PageId, Maybe ParagraphId, Double)]
        attachParagraphs irname entityRanking
          | Just results <- lookup irname retrievalResults =
             let psgOf :: HM.HashMap PageId ParagraphId
                 psgOf = HM.fromListWith (\x _ -> x)
                         $ foldMap (\(EdgeDoc{..}, _) -> fmap (\x -> (x, edgeDocParagraphId)) (HS.toList edgeDocNeighbors ++ [edgeDocArticleId]))
                         $ results
             in fmap (\(entity, score) -> (entity, (entity `HM.lookup` psgOf), score)) entityRanking

        computeRankings' :: [(Method, [(PageId, Maybe ParagraphId, Double)])]
        computeRankings' =
            [ (Method gname ename wname rname irname, attachParagraphs irname $ graphRanking graph )
            | ((gname, ename, wname, irname), graph) <- fancyWeightedGraphs,
              (rname, graphRanking) <- graphRankings
            ]
    in computeRankings'




computeSimpleGraphs :: UniverseGraph
                    -> HS.HashSet PageId
                    -> [((GraphNames, EdgeFilteringNames, WeightingNames), Graph PageId Double)]

computeSimpleGraphs universeGraph queryPageIds =
    let universeSubset ::  HM.HashMap PageId [EdgeDoc]
        universeSubset = universeGraph

        edgeDocsSubset :: [EdgeDoc]
        edgeDocsSubset =
            HS.toList $ HS.fromList
          $ filter isNotFromQueryPage
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
    => PageBundle
    -> WordEmbedding n
    -> [((GraphNames, EdgeFilteringNames, WeightingNames), Graph PageId Double)]
    -> HS.HashSet PageId
    -> [(Method, [(PageId, Double)])]

computeFullgraphRankingsForQuery pageBundle wordEmbedding simpleWeightedGraphs = \seeds ->
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
    nodeAttributes = nodesToWordVector pageBundle wordEmbedding (foldMap (nodeSet . snd) simpleWeightedGraphs)



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

dummyInvalidParagraphId :: ParagraphId
dummyInvalidParagraphId = packParagraphId "__invalid__"


logMsg :: CarRun.QueryId -> Method -> String -> IO ()
logMsg queryId method t = T.putStr $ (CarRun.unQueryId queryId)<>"\t"<>T.pack (showMethodName method)<>"\t"<>T.pack t<>"\n"


timeIt :: IO a -> IO (Double, a)
timeIt action = do
      t0 <- getCurrentTime
      !r <- action
      t1 <- getCurrentTime
      let dt = t1 `diffUTCTime` t0
      return (realToFrac dt / 60 :: Double, r)

logTimed :: CarRun.QueryId -> Method -> String -> IO a -> IO a
logTimed queryId method msg doIt = do
      logMsg queryId method msg
      (t, r) <- timeIt doIt
      logMsg queryId method $ msg<>"\ttime="<>(showFFloat (Just 3) t "")
      return r

retrieveEntities :: EntityIndex -> IO ([Term] -> [(Log Double, PageId)])
retrieveEntities entityIndexFile = do
    entityIndex <- Index.open entityIndexFile
    let model = BM25.bm25 $ BM25.sensibleParams
    -- let model = QL.queryLikelihood (QL.Dirichlet 100)
    return $ sortBy Index.descending . Index.score entityIndex model


filterOutSeeds :: QueryDoc -> [(PageId, Maybe ParagraphId, Double)] -> [(PageId, Maybe ParagraphId, Double)]
filterOutSeeds query ranking = filter notSeedEntity ranking      --  remove seed entities from ranking
  where notSeedEntity (entityId, _, _) =
             (not $ entityId `HS.member` queryDocLeadEntities query)
          && (not $ entityId == queryDocPageId query)
          
main :: IO ()
main = do
    hSetBuffering stdout LineBuffering

    (articlesFile, outputFilePrefix, embeddingsFile, querySrc, runMethods, expansionHops, simplirIndexFilepath,
      rankingType, graphset, queryRestriction, dotFilenameMaybe, numResults) <-
        execParser' 1 (helper <*> opts) mempty
    putStrLn $ "# Pages: " ++ show articlesFile
    articlesBundle <- CAR.openPageBundle articlesFile
    putStrLn $ "# Running methods: " ++ show runMethods
    putStrLn $ "# Query restriction: " ++ show queryRestriction
    putStrLn $ "# Edgedoc index: "++ show simplirIndexFilepath
    putStrLn $ "# RankingType: " ++ show rankingType

    SomeWordEmbedding wordEmbeddings <- readGlove embeddingsFile
    putStrLn $ "# Embedding: " ++ show embeddingsFile ++ ", dimension=" ++ show (wordEmbeddingDim wordEmbeddings)

    let seedMethod :: SeedDerivation -> IO (QueryDoc -> QueryDoc)
        seedMethod SeedsFromLeadSection = return id
        seedMethod (SeedsFromEntityIndex entityIndexFile) = do
            retrieve <- retrieveEntities entityIndexFile
            putStrLn $ "# entity index: " ++ show entityIndexFile
            let entitiesFromIndex :: QueryDoc -> QueryDoc
                entitiesFromIndex qdoc =
                    qdoc { queryDocLeadEntities = seeds }
                  where
                    queryTerms = textToTokens' $ queryDocQueryText qdoc
                    seeds = HS.fromList $ map snd
                            $ take 5 $ retrieve queryTerms
            return entitiesFromIndex

    queriesWithSeedEntities' <-
        case querySrc of
          QueriesFromCbor queryFile queryDeriv seedDerivation -> do
              populateSeeds <- seedMethod seedDerivation
              queryBundle <- openPageBundle queryFile
              return $ map populateSeeds $ pagesToQueryDocs queryBundle queryDeriv


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
        retrieveDocs model = map swap . collectTopK numResults  . Index.score index model
    putStrLn "# opened edgedoc index"

    handles <- sequence $ M.fromList  -- todo if we run different models in parallel, this will overwrite previous results.
      [ (method, openFile (outputFilePrefix ++ showMethodName method ++ ".run") WriteMode >>= newMVar)
      | method <- fromMaybe allMethods runMethods ]
        :: IO (M.Map Method (MVar Handle))

    ncaps <- getNumCapabilities
    let --forM_' = forM_
        forM_' = forConcurrentlyN_ ncaps

        runMethod :: CarRun.QueryId -> QueryDoc -> Method -> [(PageId, Maybe ParagraphId, Double)] -> IO ()
        runMethod queryId query method ranking = handle onError $ do
            let Just hdl = M.lookup method handles

            logMsg queryId method $ "evaluating"
            --logTimed "evaluating graph" $ evaluate $ rnf graph
            --logMsg $ "graph size: "++show (graphSize graph)
            --ranking <- logTimed "computing ranking" $ evaluate $ force $ computeRanking graph
            logMsg queryId method $ "ranking entries="++show (length ranking)
            ranking' <-
                case filter (\(entity, _ , _) -> ((/=0) $ length $ unpackPageId entity)) ranking of
                  [] -> do -- replace empty rankings with dummy result (for trec_eval)
                           logMsg queryId method $ "empty result set replaced by dummy result"
                           pure [(dummyInvalidPageId, Just dummyInvalidParagraphId, 0.0)]
                  r  -> pure r

            -- Drop seed entities
            let ranking'' =
                  case querySrc of
                    QueriesFromCbor _ _ SeedsFromLeadSection -> filterOutSeeds query ranking'
                    _                                        -> ranking'

                formatted =
                    case rankingType of
                      EntityPassageRanking ->
                        WriteRanking.formatEntityPassageRankings
                                     (T.pack $ show method)
                                     (CarRun.unQueryId queryId)
                                     $ ranking''
                      EntityRanking ->
                        WriteRanking.formatEntityRankings
                                     (T.pack $ show method)
                                     (CarRun.unQueryId queryId)
                                     (map (\(a,_,c) -> (a,c)) ranking'')
            bracket (takeMVar hdl) (putMVar hdl) $ \ h -> do
                TL.hPutStrLn h formatted
                hFlush h
          where
            onError (SomeException exc) =
                putStrLn $ concat [ "error: exception while running "
                                  , show method, " on ", show queryId, ": ", show exc ]

    let subgraphExpansion =
            forM_' queriesWithSeedEntities $ \query@QueryDoc{queryDocQueryId=queryId, queryDocPageId=queryPage, queryDocLeadEntities=seedEntities} -> do
                when (null $ seedEntities) $
                    T.putStr $ T.pack $ "# Query with no lead entities: "++show query++"\n"

                T.putStr $ T.pack $ "# Processing query "++ show query++": seeds=" ++ show seedEntities ++ "\n"
                let rankings :: [(Method, [(PageId, Maybe ParagraphId,  Double)])]
                    rankings = computeRankingsForQuery retrieveDocs articlesBundle queryId queryPage (queryDocRawTerms query) seedEntities expansionHops

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
                mapM_ (uncurry $ runMethod queryId query)
                    $ filter (filterMethods . fst) rankings

    case graphset of
      Fullgraph -> fail "Fullgraph not supported anymore "
      Subgraph ->  subgraphExpansion

    mapM_ (\h -> takeMVar h >>= hClose) handles

forConcurrentlyN_ :: Foldable f => Int -> f a -> (a -> IO ()) -> IO ()
forConcurrentlyN_ n = flip $ mapConcurrentlyL_ n
