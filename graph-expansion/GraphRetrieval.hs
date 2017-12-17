{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}

import Control.Monad (when)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Control.Exception
import Data.List (sortBy)
import Data.Tuple
import Data.Semigroup hiding (All, Any, option)
import Data.Foldable
import Data.Coerce
import Options.Applicative
import System.IO
import Data.Aeson
import Numeric.Log

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL

import CAR.Types
import CAR.Retrieve as Retrieve
import qualified CAR.RunFile as CarRun

import Graph
import EdgeDocCorpus
import WriteRanking
import GraphExpansion
import GraphExpansionExperiments
import qualified SimplIR.SimpleIndex as Index
import qualified SimplIR.SimpleIndex.Models.BM25 as BM25

type EntityIndex = Index.OnDiskIndex Term PageId Int

data SeedDerivation =  SeedsFromEntityIndex EntityIndex
                     -- todo | seedsFromQuery

newtype QuerySource = QueriesFromJson FilePath

data Graphset = Subgraph
data RankingType = EntityRanking
  deriving (Show)

graphset = Subgraph
rankingType = EntityRanking
expansionHops = 3

opts :: Parser ( FilePath,  QuerySource
               , Index.OnDiskIndex Term EdgeDoc Int
               , Index.OnDiskIndex Term Entity Int
               , Method
               )
opts =
    (,,,,)
    <$> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")
    <*> querySource
    <*> option (Index.OnDiskIndex <$> str)
               (short 'i' <> long "index" <> metavar "INDEX" <> help "simplir edgedoc index")
    <*> option (Index.OnDiskIndex <$> str)
               (short 'e' <> long "entityindex" <> metavar "EINDEX" <> help "simplir entity index")

    <*> method
    where
      method:: Parser Method
      method = Method
        <$> pure Top2000PerGraph
        <*> pure Unfiltered
        <*> option auto (short 'w' <> long "edge-weight" <> help ("edge weight function; one of "++show [minBound :: WeightingNames .. maxBound])) --  parses one of EdgeFilteringNames
        <*> option auto (short 'g' <> long "graph-walk" <> help ("graph walk algorithm; one of "++ show [minBound :: GraphRankingNames .. maxBound])) -- parses GraphRankingName
        <*> pure Bm25

      --method :: Parser Method
      -- method =  pure $ Method Top2000PerGraph Unfiltered RecipRank PageRank Bm25
--       methodMap = M.fromList [ (showMethodName m, m) | m <- allMethods ]

      querySource :: Parser QuerySource
      querySource =
               option (fmap QueriesFromJson str) (short 'j' <> long "queries-json" <> metavar "JSON" <> help "Queries from JSON")


main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    (outputFilePrefix,  querySrc, simplirIndexFilepath, entityIndexFile, method) <-
        execParser $ info (helper <*> opts) mempty
    putStrLn $ "# Running methods: " ++ show method
    putStrLn $ "# Edgedoc index: "++ show simplirIndexFilepath
    putStrLn $ "# Edgedoc index: "++ show entityIndexFile
    putStrLn $ "# RankingType: " ++ show rankingType

    let seedMethod :: SeedDerivation -> IO (QueryDoc -> QueryDoc)
--         seedMethod SeedsFromLeadSection = return id
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

    queriesWithSeedEntities <-
        case querySrc of
--           QueriesFromCbor queryFile queryDeriv seedDerivation -> do
--               populateSeeds <- seedMethod seedDerivation
--               map populateSeeds . pagesToQueryDocs siteId resolveRedirect queryDeriv
--                   <$> readPagesFile queryFile

          QueriesFromJson queryFile -> do
              QueryDocList queriesWithSeedEntities <- either error id . Data.Aeson.eitherDecode <$> BSL.readFile queryFile
              return queriesWithSeedEntities

    putStrLn $ "# query count: " ++ show (length queriesWithSeedEntities)

    index <- Index.open simplirIndexFilepath
    let retrieveDocs :: Index.RetrievalModel Term EdgeDoc Int -> RetrievalFunction EdgeDoc
        retrieveDocs model = map swap . Index.score index model
    putStrLn "# opened edgedoc index"

    handles <- sequence $ M.fromList  -- todo if we run different models in parallel, this will overwrite previous results.
      [ (method, openFile (outputFilePrefix ++ showMethodName method ++ ".run") WriteMode >>= newMVar)
      ]-- | method <- fromMaybe allMethods runMethods ]

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
--             ranking' <-
--                 case filter (\(entity, _ , _) -> ((/=0) $ length $ unpackPageId entity)) ranking of
--                   [] -> do -- replace empty rankings with dummy result (for trec_eval)
--                            logMsg queryId method $ "empty result set replaced by dummy result"
--                            pure [(dummyInvalidPageId, Just dummyInvalidParagraphId, 0.0)]
--                   r  -> pure r

            -- Drop seed entities
--             let ranking'' =
--                   case querySrc of
--                     QueriesFromCbor _ _ SeedsFromLeadSection -> filterOutSeeds query ranking
--                     _                                        -> ranking
            let
                formatted =
                    case rankingType of
--                       EntityPassageRanking ->
--                         WriteRanking.formatEntityPassageRankings
--                                      (T.pack $ show method)
--                                      (CarRun.unQueryId queryId)
--                                      $ ranking
                      EntityRanking ->
                        WriteRanking.formatEntityRankings
                                     (T.pack $ show method)
                                     (CarRun.unQueryId queryId)
                                     (map (\(a,_,c) -> (a,c)) ranking)
            bracket (takeMVar hdl) (putMVar hdl) $ \ h ->
                TL.hPutStrLn h formatted
          where
            onError (SomeException exc) =
                putStrLn $ concat [ "error: exception while running "
                                  , show method, " on ", show queryId, ": ", show exc ]

    let subgraphExpansion = --do
            forM_' queriesWithSeedEntities $ \query@QueryDoc{queryDocQueryId=queryId, queryDocPageId=queryPage, queryDocLeadEntities=seedEntities} -> do
                when (null $ seedEntities) $
                    T.putStr $ T.pack $ "# Query with no lead entities: "++show query++"\n"

                T.putStr $ T.pack $ "# Processing query "++ show query++": seeds=" ++ show seedEntities ++ "\n"
                let rankings :: [(Method, [(PageId, Maybe ParagraphId,  Double)])]
                    rankings = computeRankingsForQuery retrieveDocs  queryId queryPage (queryDocRawTerms query) seedEntities expansionHops

                let methodsAvailable = S.fromList (map fst rankings)
                    filterMethods :: Method -> Bool
                    filterMethods m = m == method
                mapM_ (uncurry $ runMethod queryId query)
                    $ filter (filterMethods . fst) rankings



    case graphset of
      --Fullgraph -> fullgraphExpansion
      Subgraph ->  subgraphExpansion

    mapM_ (\h -> takeMVar h >>= hClose) handles

forConcurrentlyN_ :: Foldable f => Int -> f a -> (a -> IO ()) -> IO ()
forConcurrentlyN_ n xs f = do
    sem <- atomically $ newTSem n
    let run x = bracket (atomically $ waitTSem sem) (const $ atomically $ signalTSem sem) (const $ f x)
    forConcurrently_ xs run


retrieveEntities :: EntityIndex -> IO ([Term] -> [(Log Double, PageId)])
retrieveEntities entityIndexFile = do
    entityIndex <- Index.open entityIndexFile
    let model = BM25.bm25 $ BM25.sensibleParams
    -- let model = QL.queryLikelihood (QL.Dirichlet 100)
    return $ sortBy Index.descending . Index.score entityIndex model




computeRankingsForQuery :: 
                          (Index.RetrievalModel Term EdgeDoc Int -> RetrievalFunction EdgeDoc)
                        -> CarRun.QueryId -> PageId ->  [Term] -> HS.HashSet PageId -> Int
                        -> [(Method, [(PageId, Maybe ParagraphId, Double)])]

computeRankingsForQuery
      retrieveDocs
      queryId queryPageId query seeds radius  =
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
        fancyGraphs = concat [--(Top5PerNode,     const $ filterGraphByTop5NodeEdges  retrieveDocs      query)
                               [ (Top100PerGraph,   irname, addSeedNodes seeds $ filterGraphByTopNGraphEdges retrievalResult 100)
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


logMsg :: CarRun.QueryId -> Method -> String -> IO ()
logMsg queryId method t = T.putStr $ (CarRun.unQueryId queryId)<>"\t"<>T.pack (showMethodName method)<>"\t"<>T.pack t<>"\n"


