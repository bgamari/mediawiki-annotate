{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

import Control.Concurrent.Async
import Control.DeepSeq hiding (rwhnf)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Parallel.Strategies
import Control.Lens (each)
import Data.Tuple
import Data.Semigroup hiding (All, Any, option)
import Options.Applicative
import System.IO
import Data.Aeson
import System.Random
import GHC.Generics
import GHC.Stack

import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG
import qualified Text.PrettyPrint.Leijen.Text as PP
import Data.List
import Data.Maybe
import Data.Foldable as Foldable
import Data.Hashable
import Control.Concurrent
import Control.Concurrent.Map
import Data.List.Split

import CAR.Types hiding (Entity)
import CAR.ToolVersion
import CAR.Retrieve as Retrieve
import qualified CAR.RunFile as CarRun
import CAR.TocFile as Toc
import CAR.Utils

import GraphExpansion
import DenseMapping
import PageRank
import qualified SimplIR.SimpleIndex as Index
import SimplIR.LearningToRank
import SimplIR.LearningToRankWrapper
import qualified SimplIR.FeatureSpace as F
--import SimplIR.FeatureSpace (featureDimension, FeatureSpace, FeatureVec, featureNames, mkFeatureSpace, concatSpace, concatFeatureVec)
import SimplIR.FeatureSpace.Normalise
import SimplIR.Intern

import qualified CAR.RunFile
import qualified SimplIR.Format.QRel as QRel
import qualified SimplIR.Ranking as Ranking
import MultiTrecRunFile
import PageRank
--import DenseMapping
import Graph

import qualified Data.GraphViz as Dot
import qualified Data.GraphViz.Attributes.Complete as Dot
import qualified Data.GraphViz.Commands.IO as Dot
import Control.Monad

import GridFeatures
import EdgeDocCorpus
import CandidateGraph
import NodeAndEdgeFeatures
import TrainAndStore

import Debug.Trace  as Debug

type NumResults = Int

type EntityIndex = Index.OnDiskIndex Term PageId Int

data SeedDerivation = SeedsFromLeadSection
                    | SeedsFromEntityIndex EntityIndex
                    -- | SeedsFromHeadingEntityLinks -- TODO

data QuerySource = QueriesFromCbor FilePath QueryDerivation SeedDerivation
                 | QueriesFromJson FilePath

data RankingType = EntityRanking | EntityPassageRanking
  deriving (Show)

data ModelSource = ModelFromFile FilePath -- filename to read model from
                 | GraphWalkModelFromFile FilePath -- filename to read model from for graph walks
                 | TrainModel FilePath -- filename to write resulting file to
                 | GraphWalkTrainModel FilePath -- filename to read model from
  deriving (Show)

data ExperimentSettings = AllExp | NoEdgeFeats | NoEntityFeats | AllEdgeWeightsOne | JustAggr | NoAggr | JustScore | JustRecip | LessFeatures | JustNone | JustSimpleRm | JustTitleAndSectionPath
                        | ExpPage | ExpSection
  deriving (Show, Read, Ord, Eq, Enum, Bounded)

data PageRankExperimentSettings = PageRankNormal | PageRankJustStructure | PageRankWeightOffset1 | PageRankWeightOffset01
  deriving (Show, Read, Ord, Eq, Enum, Bounded)

data PageRankConvergence = L2Convergence | Iteration10 | Iteration2
  deriving (Show, Read, Ord, Eq, Enum, Bounded)

data PosifyEdgeWeights = Exponentiate | ExpDenormWeight | Linear | Logistic | CutNegative
  deriving (Show, Read, Ord, Eq, Enum, Bounded)

data GraphWalkModel = PageRankWalk | BiasedPersPageRankWalk
  deriving (Show, Read, Ord, Eq, Enum, Bounded)


-- | PageRank teleportation \(\alpha\)
type TeleportationProb = Double


opts :: Parser ( FilePath
               , FilePath
               , QuerySource
               , [CarRun.QueryId]
               , NumResults
               , [(GridRun, EntityOrEdge, FilePath)]
               , Toc.IndexedCborPath ParagraphId EdgeDoc
               , FilePath
               , ModelSource
               , PosifyEdgeWeights
               , TeleportationProb
               , [ExperimentSettings]
               , PageRankExperimentSettings
               , PageRankConvergence
               , GraphWalkModel
               , Maybe MiniBatchParams
               )
opts =
    (,,,,,,,,,,,,,,,)
    <$> argument str (help "articles file" <> metavar "ANNOTATIONS-FILE")
    <*> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")
    <*> querySource
    <*> many (option (CarRun.QueryId . T.pack <$> str) (long "query" <> metavar "QUERY" <> help "execute only this query"))
    <*> option auto (short 'k' <> long "num-results" <> help "use number of results of input rankings (per query)")
    <*> some gridRunParser
    <*> (option (Toc.IndexedCborPath <$> str)  ( long "edge-doc-cbor" <> metavar "EdgeDoc-CBOR" <> help "EdgeDoc cbor file"))
    <*> (option str (long "qrel" <> metavar "QRel-FILE"))
    <*> modelSource
    <*> option auto (long "posify" <> metavar "OPT" <> help ("Option for how to ensure positive edge weights. Choices: " ++(show [minBound @PosifyEdgeWeights .. maxBound])) <> value Exponentiate)
    <*> option auto (long "teleport" <> help "teleport probability (for page rank)" <> value 0.1)
    <*> many (option auto (long "exp" <> metavar "EXP" <> help ("one or more switches for experimentation. Choices: " ++(show [minBound @ExperimentSettings .. maxBound]))))
    <*> option auto (long "pagerank-settings" <> metavar "PREXP" <> help ("Option for how to ensure positive edge weights. Choices: " ++(show [PageRankNormal,PageRankJustStructure,  PageRankWeightOffset1, PageRankWeightOffset01])) <> value PageRankNormal)
    <*> option auto (long "pagerank-convergence" <> metavar "CONV" <> help ("How pagerank determines convergence. Choices: " ++(show [minBound @PageRankConvergence .. maxBound])) <> value Iteration10)
    <*> option auto (long "graph-walk-model" <> metavar "PAGERANK" <> help ("Graph walk model. Choices: " ++(show [minBound @GraphWalkModel .. maxBound])) <> value PageRankWalk)
    <*> optional minibatchParser
    where

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

      modelSource :: Parser ModelSource
      modelSource =
            option (TrainModel <$> str) (long "train-model" <> metavar "Model-FILE" <> help "train learning-to-rank model and write to Model-FILE")
        <|> option (ModelFromFile <$> str) (long "test-model" <> metavar "Model-FILE" <> help "read learning-to-rank model from Model-FILE")
        <|> option (GraphWalkModelFromFile <$> str) (long "read-model" <> metavar "Model-FILE" <> help "read learning-to-rank model for graph walking from Model-FILE")
        <|> option (GraphWalkTrainModel <$> str) (long "train-walk-model" <> metavar "Model-FILE" <> help "train learning-to-rank model for graph walking from Model-FILE")




-- --------------------------------- Query Doc ------------------------------------------------------


data QueryDoc = QueryDoc { queryDocQueryId      :: !CarRun.QueryId
                         , queryDocQueryText    :: !T.Text
                         }
           deriving (Show, Generic, Eq)
instance FromJSON QueryDoc
instance ToJSON QueryDoc

data QueryDocList = QueryDocList { queryDocListContent :: [QueryDoc]}
           deriving Generic
instance FromJSON QueryDocList
instance ToJSON QueryDocList

data QueryDerivation = QueryFromPageTitle | QueryFromSectionPaths

pagesToQueryDocs :: QueryDerivation
                 -> [Page]
                 -> [QueryDoc]
pagesToQueryDocs deriv pages =
    queryDocs
  where
    queryDocs = case deriv of
      QueryFromPageTitle ->
          [ QueryDoc { queryDocQueryId      = CarRun.pageIdToQueryId $  pageId page
                     , queryDocQueryText    = getPageName $ pageName page
                     }
          | page <- pages
          ]
      QueryFromSectionPaths ->
          [ QueryDoc { queryDocQueryId      = CarRun.sectionPathToQueryId sectionPath
                     , queryDocQueryText    = T.unwords
                                            $ getPageName (pageName page) : getPageName (pageName page) -- twice factor
                                              : map getSectionHeading headings
                     }
          | page <- pages
          , (sectionPath, headings, _) <- pageSections page
          ]


-- ---------------------------------------------------------------------------------------

(>!<) :: (Show k, Ord k, HasCallStack) => M.Map k v -> k -> v
m >!< key =
    case key `M.lookup` m  of
        Just v -> v
        Nothing -> error $ ">!<: Can't lookup key "<> show key <> " in map. Map size: "<> show (length m) <>" Example keys " <> (show $ take 10 $ M.keys m)<> "..."

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering

    (articlesFile, outputFilePrefix, querySrc,
      queryRestriction, numResults, gridRunFiles
      , edgeDocsCborFile
      , qrelFile, modelSource
      , posifyEdgeWeightsOpt,  teleportation, experimentSettings
      , pageRankExperimentSettings, pageRankConvergence, graphWalkModel
      , miniBatchParamsMaybe  ) <- execParser' 1 (helper <*> opts) mempty
    putStrLn $ "# Pages: " ++ show articlesFile
    putStrLn $ "# Query restriction: " ++ show queryRestriction

    let entityRunFiles  = [ (g, r) | (g, Entity, r) <- gridRunFiles]
        edgedocRunFiles = [ (g, r) | (g, Edge, r) <- gridRunFiles]

    putStrLn $ "# Entity runs:  "++ (show $ fmap (show) (entityRunFiles ))
    putStrLn $ "# EdgeDoc runs: "++ ( show $ fmap (show) (edgedocRunFiles))
    putStrLn $ "# numResults: "++ ( show (numResults))

    putStrLn $ " Experimentation settings: "++ (show experimentSettings)
    putStrLn $ " model comes from : "++ (show modelSource)
    putStrLn $ " teleport (only for page rank) : "++ (show teleportation)
    putStrLn $ " posify with (only for page rank) : "++ (show posifyEdgeWeightsOpt)
    putStrLn $ " pageRankExperimentSettings (only for page rank) : "++ (show pageRankExperimentSettings)
    putStrLn $ " graphWalkModel (only for page rank) : "++ (show graphWalkModel)
    putStrLn $ " MinbatchParams (only for training) : "++ (show miniBatchParamsMaybe)

    let miniBatchParams = fromMaybe defaultMiniBatchParams miniBatchParamsMaybe


    queries' <-
        case querySrc of
          QueriesFromCbor queryFile queryDeriv _seedDeriv -> do
              pagesToQueryDocs queryDeriv <$> readPagesOrOutlinesAsPages queryFile

          QueriesFromJson queryFile -> do
              QueryDocList queries <- either error id . Data.Aeson.eitherDecode <$> BSL.readFile queryFile
              return queries

    let dotFileName :: QueryId -> FilePath
        dotFileName queryId = outputFilePrefix ++ "-"++ T.unpack (CAR.RunFile.unQueryId queryId) ++"-graphviz.dot"

        filterGraphTopEdges :: Graph PageId Double -> Graph PageId Double
        filterGraphTopEdges graph =  graph -- filterEdges (\_ _ weight -> weight > 5.0) graph

    let fixQRel :: QRel.Entry QRel.QueryId QRel.DocumentName QRel.IsRelevant
                -> QRel.Entry CAR.RunFile.QueryId QRel.DocumentName QRel.IsRelevant
        fixQRel (QRel.Entry qid docId rel) = QRel.Entry (CAR.RunFile.QueryId qid) docId rel
    qrel <- map fixQRel <$> QRel.readQRel @IsRelevant qrelFile


    let qrelMap :: M.Map QueryId (M.Map PageId IsRelevant)
        qrelMap =
          M.unionsWith (M.unionWith checkSame)
          [ M.singleton qid (M.singleton docId' rel)
          | QRel.Entry qid docId rel <- qrel
          , let docId' = packPageId $ T.unpack docId
          ]
          where checkSame :: IsRelevant -> IsRelevant -> IsRelevant
                checkSame rel1 rel2 =
                        if rel1 == rel2 then rel1
                        else error $ "Qrel file contains conflicting relevance data for (query, item): "<> qrelFile


        lookupQrel :: IsRelevant -> QueryId -> PageId -> IsRelevant
        lookupQrel def query document =
            fromMaybe def $ do
                m <- query `M.lookup` qrelMap
                r <- document `M.lookup` m
                return r


    queries <-
        if null queryRestriction
          then return queries'
          else do putStrLn $ "# using only queries "<>show queryRestriction
                  return
                    $ nub
                    $ filter (\q-> queryDocQueryId q `elem` queryRestriction) queries'
    putStrLn $ "# query count: " ++ show (length queries)

    putStrLn "Loading edgeDocsLookup."
    edgeDocsLookup <- readEdgeDocsToc edgeDocsCborFile

    ncaps <- getNumCapabilities

    putStrLn "Loading EntityRuns..."
    entityRuns <- fmap concat $ mapConcurrentlyL ncaps
        (runInternM . runInternM . mapM (mapM (\path ->
                     lift . internAll (each . CAR.RunFile.document)
                 =<< internAll (each . CAR.RunFile.traverseText (const pure))
                 =<< liftIO (CAR.RunFile.readEntityRun path))))
        (chunksOf 2 entityRunFiles)                                              -- todo why chunks of 2?!?
        :: IO [(GridRun, [RankingEntry PageId])]

    putStrLn $ "Loaded EntityRuns: "<> show (length entityRuns)

    putStrLn "Loading EdgeRuns..."
    edgeRuns <- fmap concat $ mapConcurrentlyL ncaps
        (runInternM . runInternM . mapM (mapM (\path ->
                     lift . internAll (each . CAR.RunFile.document)
                 =<< internAll (each . CAR.RunFile.traverseText (const pure))
                 =<< liftIO (CAR.RunFile.readParagraphRun path))))
        (chunksOf 2 edgedocRunFiles)                                             -- todo why chunks of 2?!?
        :: IO [(GridRun, [RankingEntry ParagraphId])]

    putStrLn $ "Loaded EdgeRuns: "<> show (length edgeRuns)

    putStrLn "Computing collapsed runs..."
    let collapsedEntityRun :: M.Map QueryId [MultiRankingEntry PageId GridRun]
        !collapsedEntityRun =
            collapseRuns
            $ map (fmap $ filter (\entry -> CAR.RunFile.carRank entry <= numResults)) entityRuns
        !collapsedEdgedocRun =
            collapseRuns
            $ map (fmap $ filter (\entry -> CAR.RunFile.carRank entry <= numResults)) edgeRuns

        tr x = traceShow x x
    putStrLn "Computed collapsed runs."
    putStrLn $ "queries from collapsed entity runs: "++show (M.size collapsedEntityRun)
    putStrLn $ "queries from collapsed edge doc runs: "++show (M.size collapsedEdgedocRun)

    let candidateGraphGenerator = selectGenerousCandidateGraph edgeDocsLookup
    -- predict mode
    -- alternative: load model from disk, then use graph feature vectors to produce a graph with edge weights (Graph PageId Double)
    -- use pagerank on this graph to predict an alternative node ranking
    -- save predicted node ranking as run-file

    let   graphWalkRanking :: QueryId -> WeightVec EdgeFeature -> _ -> M.Map QueryId (HM.HashMap PageId Double) ->  [Eigenvector PageId Double]
          graphWalkRanking query params' edgeFSpace' nodeDistr
                 | any (< 0) graph' = error ("negative entries in graph' for query "++ show query ++ ": "++ show (count (< 0) graph'))
                 | otherwise =
--                     let eigv = pageRankConvergence walkIters
--                     putStrLn $ show query <> " -> " <> show (take 2 $ toEntries eigv)
                   -- exportGraphViz (filterGraphTopEdges $ dropDisconnected graph') (dotFileName query)
--                     in Ranking.fromList $ map swap $ toEntries eigv
                    walkIters
                --                 | otherwise = traceShow (take 3 $ toEntries eigv) $ Ranking.fromList $ map swap $ toEntries eigv
              where
                  count predicate = getSum . foldMap f
                    where f x = if predicate x then Sum 1 else Sum 0

                  candidates = candidateGraphGenerator query edgeRun entityRun -- selectCandidateGraph edgeDocsLookup query edgeRun entityRun
                    where
                      edgeRun = collapsedEdgedocRun >!< query
                      entityRun = collapsedEntityRun >!< query

                  graph :: Graph PageId EdgeFeatureVec
                  graph =  fmap (filterExpSettings edgeFSpace')
                         $ generateEdgeFeatureGraph query candidates -- edgeDocsLookup query edgeRun entityRun


                  normalizer :: Normalisation _ Double
                  normalizer = zNormalizer $ Foldable.toList graph


                  graph' :: Graph PageId Double
                  graph' = fmap (posifyDot pageRankExperimentSettings posifyEdgeWeightsOpt normalizer params' (Foldable.toList graph)) graph
                  -- for debugging...
                --                   graph' = fmap (\feats -> trace (show feats) ( tr  ( posifyDot params feats))) graph
                    where
                      normFeats :: EdgeFeatureVec -> EdgeFeatureVec
                      normFeats fv = normFeatures normalizer fv
                  walkIters :: [Eigenvector PageId Double]
                  walkIters = case graphWalkModel of
                                PageRankWalk -> pageRank teleportation graph'
                                BiasedPersPageRankWalk -> persPageRankWithNonUniformSeeds (teleportation/2) seedNodeDistr graph'
                                  where  betaTotal = teleportation/2
                                         seedNodeDistr = fmap (* betaTotal) (nodeDistr M.! query )



    let   makeFeatureGraphs :: F.FeatureSpace EdgeFeature ->  M.Map QueryId (Graph PageId EdgeFeatureVec)
          makeFeatureGraphs edgeFSpace' =
              M.fromList [ (qid, f edgeFSpace' qid)
                         | q <- queries
                         , let qid = queryDocQueryId q
                         ]
            where
              f :: F.FeatureSpace EdgeFeature -> QueryId -> Graph PageId EdgeFeatureVec
              f edgeFSpace' query =
                  fmap (filterExpSettings edgeFSpace')
                  $ generateEdgeFeatureGraph query candidates -- edgeDocsLookup query edgeRun entityRun
                where
                  !candidates = candidateGraphGenerator query edgeRun entityRun -- selectCandidateGraph edgeDocsLookup query edgeRun entityRun
                    where
                      edgeRun = collapsedEdgedocRun >!< query
                      entityRun = collapsedEntityRun >!< query


          produceWalkingGraph :: M.Map QueryId (Graph PageId EdgeFeatureVec)
                              -> Eigenvector PageId Double
                              -> QueryId
                              -> F.FeatureSpace EdgeFeature
                              -> M.Map QueryId (HM.HashMap PageId Double)
                              -> WeightVec EdgeFeature
                              -> (Ranking Double PageId, Eigenvector PageId Double)
          produceWalkingGraph featureGraphs initialEigv query edgeFSpace' nodeDistr =
              \params ->
                let graph = walkingGraph params
                in nextRerankIter params initialEigv
              where
                !featureGraph = featureGraphs  >!< query

                normalizer :: Normalisation _ Double
                !normalizer = zNormalizer $ Foldable.toList featureGraph

                walkingGraph :: WeightVec EdgeFeature -> Graph PageId Double
                walkingGraph params' =
                    let graph = fmap (posifyDot pageRankExperimentSettings posifyEdgeWeightsOpt normalizer params' (Foldable.toList featureGraph)) featureGraph
                        graph' = dropLowEdges graph
--                         minEdgeWeight g = Foldable.minimum g
--                         !x = Debug.trace ("produceWalkingGraph: " <> show query <> "   minweight graph = "<>show (minEdgeWeight graph) <> " minweight graph' = "<> show (minEdgeWeight graph') ) $ 4
                    in graph'

                dropLowEdges :: Graph PageId Double -> Graph PageId Double
                dropLowEdges graph = filterEdges significantEdgeWeight graph
                                        where significantEdgeWeight :: PageId ->  PageId -> Double -> Bool
                                              significantEdgeWeight _ _ e = e > 1e-8

                walkIters :: Eigenvector PageId Double
                          -> Graph PageId Double
                          -> [Eigenvector PageId Double]
                walkIters initial graph' =
                    case graphWalkModel of
                      PageRankWalk -> pageRankWithInitial teleportation graph' initial
                      BiasedPersPageRankWalk -> -- persPageRankWithNonUniformSeeds (teleportation/2) seedNodeDistr graph'
                          biasedPersPageRankWithInitial alpha seedNodeDistr graph' initial
                  where
                    betaTotal = teleportation/2
                    seedNodeDistr = fmap (* betaTotal) (nodeDistr M.! query )
                    alpha = (teleportation/2)

                nextRerankIter :: WeightVec EdgeFeature -> Eigenvector PageId Double
                               -> (Ranking Double PageId, Eigenvector PageId Double)
                nextRerankIter params initial  =
                      let graph = walkingGraph params
                          nexteigen = (!!1) $ walkIters initial $ graph
                          ranking = eigenvectorToRanking nexteigen
--                           debugRanking = unlines $ fmap show $ take 3 $ Ranking.toSortedList ranking
                      in (ranking, nexteigen)


                -- for debugging...
              --                   graph' = fmap (\feats -> trace (show feats) ( tr  ( posifyDot params feats))) graph





    case modelSource of
      GraphWalkTrainModel modelFile -> do
          let updatedModelFile = modelFile <> "walk.json"

          putStrLn "loading model"
          Just model <-  Data.Aeson.decode @(Model CombinedFeature) <$> BSL.readFile modelFile
          let modelFeaturesFromModel = modelFeatures model

          let edgeFSpace' = F.mkFeatureSpace [ f'
                                             | f <- F.featureNames $ modelFeaturesFromModel
                                             , Right f' <- pure f
                                             ]
              featureGraphs =  makeFeatureGraphs edgeFSpace'

              nodeDistr :: M.Map QueryId (HM.HashMap PageId Double) -- only positive entries, expected to sum to 1.0
              nodeDistr = nodeDistrPriorForGraphwalk candidateGraphGenerator model collapsedEntityRun collapsedEdgedocRun

              augmentWithQrels :: QueryId -> Ranking Double PageId -> Ranking Double (PageId, IsRelevant)
              augmentWithQrels query ranking = Ranking.fromSortedList
                                       $ [ (score, (page, rel))
                                         | (score, page) <- Ranking.toSortedList ranking
                                         , let rel = lookupQrel NotRelevant query page
                                         ]

              totalRels = fmap countRel qrelMap
                            where countRel :: M.Map x IsRelevant -> Int
                                  countRel m = length [ r
                                                      | (_, r) <- M.toList m
                                                      , QRel.isPositive r
                                                      ]

              metric :: ScoringMetric IsRelevant QueryId _
              metric = meanAvgPrec (\q -> fromMaybe 0 $ q `M.lookup` totalRels)  Relevant



              someKindOfTrainingData =  M.fromList $[(q,q) | q <- intersect (M.keys totalRels) (M.keys featureGraphs) ] -- totalRels does not include queries for which there is no training data

          gen0 <- newStdGen

          let initParams' :: WeightVec EdgeFeature
              initParams' = WeightVec $ F.projectFeatureVec edgeFSpace'
                             (either (const Nothing) Just)
                             (getWeightVec $ modelWeights' model)

              initialEigenv :: Graph PageId a -> Eigenvector PageId Double
              initialEigenv graph' = PageRank.uniformInitial mapping
                where !mapping = DenseMapping.mkDenseMapping (nodeSet graph')

              iterate :: StdGen
                      -> WeightVec EdgeFeature
                      -> M.Map QueryId (Eigenvector PageId Double)
                      -> [(WeightVec EdgeFeature, M.Map QueryId (Eigenvector PageId Double), M.Map QueryId (Ranking Double (PageId, IsRelevant)))]
              iterate gen0 params eigvs =
                  let nextPageRankIter :: M.Map QueryId (WeightVec EdgeFeature -> (Ranking Double PageId, Eigenvector PageId Double))
                      nextPageRankIter = M.fromList
                               [ (qid, produceWalkingGraph featureGraphs eigv0 qid edgeFSpace' nodeDistr)
                               | q <- queries
                               , let qid = queryDocQueryId q
                                     eigv0 = eigvs >!< qid
                               ]

                      rerank :: QueryId -> WeightVec EdgeFeature -> Ranking Double (PageId, IsRelevant)
                      rerank query w =
                          let (ranking, _pageRank) = (nextPageRankIter >!< query) w
                          in augmentWithQrels query ranking


                      (gen1, gen2) = System.Random.split gen0
                      optimise :: StdGen -> WeightVec EdgeFeature -> M.Map QueryId QueryId -> [WeightVec EdgeFeature]
                      optimise gen model someKindOfTrainingData' =
                            let scoreParams = naiveCoordAscent metric rerank gen model someKindOfTrainingData'
                            in fmap snd scoreParams
                      params' :: WeightVec EdgeFeature
                      params' = (!!2) $ miniBatched 1 20 optimise gen1 params someKindOfTrainingData

                      iterResult :: M.Map QueryId (Ranking Double PageId, Eigenvector PageId Double)
                      iterResult = fmap ($ params') nextPageRankIter

                      eigvs' :: M.Map QueryId (Eigenvector PageId Double)
                      eigvs' = fmap snd iterResult
                      rankings' :: M.Map QueryId (Ranking Double (PageId, IsRelevant))
                      rankings' = M.mapWithKey (\q (r,_) -> augmentWithQrels q r) iterResult
                      score' = metric rankings'
                      printTopRanking rs =
                         unlines $ take 2 $ M.elems $ fmap ( show . take 1 . Ranking.toSortedList) rs
                      !x = Debug.trace ("trainwalk score " <> (show score') <> "\n topRankEntries "<> (printTopRanking rankings') <> "\n params' "<> show params') $ 0

                  in (params', eigvs',  rankings') : iterate gen2 params' eigvs'

              iters = iterate gen0 initParams' (fmap initialEigenv featureGraphs)
              (newParams, eigenvs, trainRanking):_ = drop 2 iters
              trainScore :: Double
              trainScore = metric trainRanking

          putStrLn $ "new model params " <> show newParams
          let model = Model newParams
              formatQuery pageId = CAR.RunFile.QueryId $ T.pack $ unpackPageId pageId

              storeRankingData' ::  FilePath
                       -> M.Map  CAR.RunFile.QueryId (Ranking Double (PageId, IsRelevant))
                       -> Double
                       -> String
                       -> IO ()
              storeRankingData' outputFilePrefix ranking metricScore modelDesc = do
                  putStrLn $ "Model "++modelDesc++" test metric "++ show (metricScore) ++ " MAP."
                  CAR.RunFile.writeEntityRun (outputFilePrefix++"-model-"++modelDesc++".run")
                       $ l2rRankingToRankEntries (CAR.RunFile.MethodName $ T.pack $ "l2r "++modelDesc)
                       $ ranking
                where l2rRankingToRankEntries methodName rankings =
                          [ CAR.RunFile.RankingEntry { carQueryId = query
                                                     , carDocument = doc
                                                     , carRank = rank
                                                     , carScore = rankScore
                                                     , carMethodName = methodName
                                                     }
                          | (query, ranking) <- M.toList rankings
                          , ((rankScore, (doc, rel)), rank) <- (Ranking.toSortedList ranking) `zip` [1..]
                          ]


          storeModelData outputFilePrefix updatedModelFile model trainScore "trainwalk"
          storeRankingData' outputFilePrefix trainRanking trainScore "trainwalk"

--
-- dumpFullModelsAndRankings
--     :: forall f. (Ord f, Show f)
--     => M.Map Q [(DocId, FeatureVec f Double, Rel)]
--     -> (Model f, Double)
--     -> ScoringMetric IsRelevant CAR.RunFile.QueryId QRel.DocumentName
--     -> FilePath
--     -> FilePath
--     -> [IO()]
-- dumpFullModelsAndRankings trainData (model, trainScore) metric outputFilePrefix modelFile =
--     let modelDesc = "train"
--         trainRanking = rerankRankings' model trainData
--     in [ storeRankingData outputFilePrefix trainRanking metric modelDesc
--        , storeModelData outputFilePrefix modelFile model trainScore modelDesc
--        ]



      GraphWalkModelFromFile modelFile -> do
          putStrLn "loading model"
          Just model <-  Data.Aeson.decode @(Model CombinedFeature) <$> BSL.readFile modelFile
          let modelFeaturesFromModel = modelFeatures model


          let nodeDistr :: M.Map QueryId (HM.HashMap PageId Double) -- only positive entries, expected to sum to 1.0
              nodeDistr = nodeDistrPriorForGraphwalk candidateGraphGenerator model collapsedEntityRun collapsedEdgedocRun

          let edgeFSpace' = F.mkFeatureSpace [ f'
                                             | f <- F.featureNames $ modelFeaturesFromModel
                                             , Right f' <- pure f
                                             ]

              params' :: WeightVec EdgeFeature
              params' = WeightVec $ F.projectFeatureVec edgeFSpace'
                        (either (const Nothing) Just)
                        (getWeightVec $ modelWeights' model)

              runRanking query = do
                  let graphRanking = eigenvectorToRanking
                                   $ graphWalkToConvergence pageRankConvergence
                                   $ graphWalkRanking query params' edgeFSpace' nodeDistr
                      rankEntries =  [ CAR.RunFile.RankingEntry query pageId rank score (CAR.RunFile.MethodName (T.pack (show graphWalkModel)))
                                    | (rank, (score, pageId)) <- zip [1..] (Ranking.toSortedList graphRanking)
                                    ]

--                       fileId qid = T.unpack $ T.replace "/" "--" qid

--                   CAR.RunFile.writeEntityRun  (outputFilePrefix ++ "-"++ fileId (CAR.RunFile.unQueryId query) ++"-pagerank-test.run")
--                                     $
                  return $ rankEntries

          rankEntries <- concat <$> mapConcurrently (runRanking . queryDocQueryId) queries
          CAR.RunFile.writeEntityRun  (outputFilePrefix ++ "-" ++ show graphWalkModel ++ "-test.run") rankEntries



      ModelFromFile modelFile -> do
          Just model <-  trace "loading model" $ Data.Aeson.decode @(Model CombinedFeature) <$> BSL.readFile modelFile

          let augmentNoQrels     :: forall docId queryId f.
                                    (Ord queryId, Ord docId)
                                 => M.Map (queryId, docId) (F.FeatureVec f Double)
                                 -> M.Map queryId [(docId, F.FeatureVec f Double, IsRelevant)]
              augmentNoQrels docFeatures =
                    let franking :: M.Map queryId [(docId, F.FeatureVec f Double, IsRelevant)]
                        franking = M.fromListWith (++)
                                   [ (qid, [(doc, features, Relevant)])
                                   | ((qid, doc), features) <- M.assocs docFeatures
                                   ]
                    in franking


          let docFeatures = makeStackedFeatures' candidateGraphGenerator (modelFeatures model) collapsedEntityRun collapsedEdgedocRun

          putStrLn $ "Made docFeatures: "<>  show (length docFeatures)
          let allData :: TrainData CombinedFeature
              allData = augmentWithQrels qrel docFeatures Relevant

--               !metric = avgMetricQrel qrel
              totalElems = getSum . foldMap ( Sum . length ) $ allData
              totalPos = getSum . foldMap ( Sum . length . filter (\(_,_,rel) -> rel == Relevant)) $ allData

          putStrLn $ "Test model with (trainData) "++ show (M.size allData) ++
                    " queries and "++ show totalElems ++" items total of which "++
                    show totalPos ++" are positive."

          let trainRanking = withStrategy (parTraversable rwhnf)
                           $ rerankRankings' model allData
          storeRankingDataNoMetric outputFilePrefix trainRanking "learn2walk-degreecentrality"



      TrainModel modelFile -> do
          let combinedFSpace' = F.mkFeatureSpace
                                $ filter (filterFeaturesByExperimentSetting experimentSettings)
                                $ F.featureNames combinedFSpace  -- Todo this is completely unsafe

          let docFeatures = makeStackedFeatures candidateGraphGenerator collapsedEntityRun collapsedEdgedocRun combinedFSpace'


          putStrLn $ "Made docFeatures: "<>  show (length docFeatures)
          let allData :: TrainData CombinedFeature
              allData = augmentWithQrels qrel docFeatures Relevant

              !metric = avgMetricQrel qrel
              totalElems = getSum . foldMap ( Sum . length ) $ allData
              totalPos = getSum . foldMap ( Sum . length . filter (\(_,_,rel) -> rel == Relevant)) $ allData

          putStrLn $ "Feature dimension: "++show (F.featureDimension $ F.featureSpace $ (\(_,a,_) -> a) $ head $ snd $ M.elemAt 0 allData)
          putStrLn $ "Training model with (trainData) "++ show (M.size allData) ++
                    " queries and "++ show totalElems ++" items total of which "++
                    show totalPos ++" are positive."

          let displayTrainData :: Show f => TrainData f -> [String]
              displayTrainData trainData =
                [ show k ++ " -> "++ show elm
                | (k,list) <- M.toList trainData
                , elm <- list]

          putStrLn $ "Training Data = \n" ++ intercalate "\n" (take 10 $ displayTrainData $ force allData)
          gen0 <- newStdGen  -- needed by learning to rank
          trainMe miniBatchParams gen0 allData combinedFSpace' metric outputFilePrefix modelFile

-- --------------------------------------

biasedPersPageRankWithInitial
    :: forall n a. (RealFloat a, VU.Unbox a, VG.Vector VU.Vector a, Eq n, Hashable n, Show n, Show a, HasCallStack)
    => a                  -- ^ teleportation probability \(\alpha\) to be uniformly distributed
    -> HM.HashMap n a     -- ^ teleportation probability \(\beta\) for each seed
    -> Graph n a          -- ^ the graph
    -> Eigenvector n a  -- ^ initial page rank vector
    -> [Eigenvector n a]  -- ^ principle eigenvector iterates
biasedPersPageRankWithInitial alpha seeds graph initial =
    persPageRankWithSeedsAndInitial mapping initial alpha seeds graph
  where
    !mapping  = mkDenseMapping (nodeSet graph)

pageRankWithInitial
    :: forall n a. (RealFloat a, VU.Unbox a, VG.Vector VU.Vector a, Eq n, Hashable n, Show n, Show a, HasCallStack)
    => a                  -- ^ teleportation probability \(\alpha\) to be uniformly distributed
    -> Graph n a          -- ^ the graph
    -> Eigenvector n a  -- ^ initial page rank vector
    -> [Eigenvector n a]  -- ^ principle eigenvector iterates
pageRankWithInitial alpha graph initial =
    persPageRankWithSeedsAndInitial mapping initial alpha HM.empty graph
  where
    !mapping  = mkDenseMapping (nodeSet graph)

graphWalkToConvergence :: PageRankConvergence -> [Eigenvector PageId Double] -> Eigenvector PageId Double
graphWalkToConvergence conv walkIters =
   let pageRankIters = zip walkIters (tail walkIters)

   in case conv of
        L2Convergence -> snd
                       $ head
                       $ dropWhile (\(x,y) -> relChange x y > 1e-3)
                       $ pageRankIters
        Iteration10   -> snd $ (!! 10)  pageRankIters
        Iteration2    -> snd $ (!! 2)  pageRankIters

eigenvectorToRanking :: Eigenvector doc Double -> Ranking Double doc
eigenvectorToRanking = Ranking.fromList . map swap . toEntries

nodeDistrPriorForGraphwalk :: CandidateGraphGenerator
                           -> Model CombinedFeature
                           -> M.Map QueryId [MultiRankingEntry PageId GridRun]
                           -> M.Map QueryId [MultiRankingEntry ParagraphId GridRun]
                           -> M.Map QueryId (HM.HashMap PageId Double)
nodeDistrPriorForGraphwalk candidateGraphGenerator model collapsedEntityRun collapsedEdgedocRun =

  let modelFeaturesFromModel = modelFeatures model
      docFeatures :: M.Map (QueryId, QRel.DocumentName) CombinedFeatureVec
      docFeatures = makeStackedFeatures' candidateGraphGenerator modelFeaturesFromModel collapsedEntityRun collapsedEdgedocRun
      degreeCentrality = fmap (modelWeights' model `score`) docFeatures
      queryToScoredList = M.fromListWith (<>) [(q, [(d, score)]) | ((q,d), score) <- M.toList degreeCentrality ]
      ranking :: M.Map QueryId (Ranking.Ranking Double QRel.DocumentName)
      ranking = fmap (Ranking.fromList . map swap) queryToScoredList

      rankingPageId :: M.Map QueryId (Ranking.Ranking Double PageId)
      rankingPageId = fmap (fmap qrelDocNameToPageId) ranking

      nodeDistr :: M.Map QueryId (HM.HashMap PageId Double) -- only positive entries, expected to sum to 1.0
      nodeDistr = fmap nodeRankingToDistribution rankingPageId
  in nodeDistr

-- --------------------------------------


qrelDocNameToPageId :: QRel.DocumentName -> PageId
qrelDocNameToPageId docname = packPageId $ T.unpack docname


filterFeaturesByExperimentSetting :: [ExperimentSettings] ->  (CombinedFeature -> Bool)
filterFeaturesByExperimentSetting settings fname =
    all (`convert` fname) settings
  where
    convert :: ExperimentSettings -> (CombinedFeature -> Bool)
    convert setting = case setting of
                    AllExp -> const True
                    NoEdgeFeats -> noEdge
                    NoEntityFeats -> noEntity
                    AllEdgeWeightsOne -> const True -- needs to be handled elsewhere
                    JustAggr -> onlyAggr
                    NoAggr -> not . onlyAggr
                    JustScore -> onlyScore
                    JustRecip -> onlyRR
                    LessFeatures -> onlyLessFeatures
                    JustNone -> onlyNoneFeatures
                    ExpPage -> onlyPage
                    ExpSection -> onlySection
                    JustSimpleRm -> onlySimpleRmFeatures
                    JustTitleAndSectionPath -> onlyTitleAndSectionPath




-- ----------------

-- ========================




dropUnjudged :: Ord q
             => M.Map q [(QRel.DocumentName, F.FeatureVec f Double, Maybe IsRelevant)]
             -> M.Map q [(QRel.DocumentName, F.FeatureVec f Double, IsRelevant)]
dropUnjudged featureMap =
    M.filter (not . null)   -- drop entries with empty lists
    $ M.map (mapMaybe dropUnjudged') featureMap
   where dropUnjudged' (doc, feat, Just rel) = Just $ (doc, feat, rel)
         dropUnjudged' (_ , _, Nothing) = Nothing


-- -----------------------------------
-- iterative optimization
-- -----------------------------------




logistic :: Double -> Double
logistic t =
    1.0 / (1.0 + exp (-1 * t))


-- | Compute a dot product between a feature and weight vector, ensuring
-- positivity.
posifyDot :: PageRankExperimentSettings -> PosifyEdgeWeights
          -> Normalisation EdgeFeature Double
          -> WeightVec EdgeFeature  -- ^ parameter vector
          -> [EdgeFeatureVec] -- ^ all features
          -> EdgeFeatureVec
          -> Double
posifyDot expSettings posifyOpt normalizer params' allFeatures =
    \feats ->
    let computedWeight =
          case posifyOpt of
              Exponentiate ->  exp (params' `score` feats)
              Logistic ->  logistic (params' `score` feats)
              CutNegative ->
                  case params' `score` feats of
                    x | x > 0.0 -> x
                    _ -> 0.0

              ExpDenormWeight ->  exp (denormWeights' `score` feats)
              Linear  -> (params' `score` feats) - minimumVal
    in case expSettings of
          PageRankNormal -> computedWeight
          PageRankJustStructure -> 1.0
          PageRankWeightOffset1 -> computedWeight + 1.0
          PageRankWeightOffset01 -> computedWeight + 0.1

  where
    !minimumVal = minimum $ fmap (\feats -> params' `score` feats) allFeatures

    denormWeights' :: WeightVec EdgeFeature
    denormWeights' =
        WeightVec $ denormWeights normalizer (getWeightVec params')

-- ---------------------------------------------
-- Graphviz export
-- ---------------------------------------------

exportGraphViz :: Graph PageId Double -> FilePath -> IO ()
exportGraphViz fancyWeightedGraph dotFilename = do
    let graph = dotGraph fancyWeightedGraph  --todo highlight seeds
    Dot.writeDotFile (dotFilename ++ ".dot") graph
    void $ Dot.runGraphvizCommand Dot.Neato graph Dot.Svg dotFilename

instance Dot.PrintDot PageId where
    unqtDot pageId = Dot.unqtDot $ unpackPageId pageId
    toDot = pure . PP.dquotes . foldMap PP.char . unpackPageId

dotGraph :: Graph PageId Double -> Dot.DotGraph PageId
dotGraph graph = Dot.graphElemsToDot params nodes edges
  where
    params = Dot.nonClusteredParams { Dot.fmtEdge = \(_,_,w) -> [ Dot.penWidth (w/10.0), Dot.Weight $ Dot.Int (ceiling w) ]
                                    , Dot.fmtNode = \(_,a) -> [Dot.toLabel a]
                                    , Dot.globalAttributes = [ Dot.GraphAttrs [ Dot.OutputOrder Dot.EdgesFirst
                                                                              , Dot.Overlap $ Dot.PrismOverlap Nothing] ]
                                    }
    nodes = [ (a, unpackPageName $ pageIdToName a) | a <- HS.toList $ nodeSet graph ]
    edges = [ (a,b,w)
            | (a, ns) <- HM.toList $ getGraph graph
            , (b, w) <- HM.toList ns
            ]

-- ---------------------------------------------
-- Node rankings to teleportation distribution
-- ---------------------------------------------

nodeRankingToDistribution :: Ranking.Ranking Double PageId
                          -> HM.HashMap PageId Double -- only positive entries, expected to sum to 1.0
nodeRankingToDistribution ranking =
    let proportions = HM.fromList
                    $ [ (node, 1.0 / (realToFrac (rank+1)))
                      | (rank, (_score, node)) <- zip [1 :: Int ..] $ Ranking.toSortedList ranking
                      , rank <= 20
                      ]
        totals = Foldable.sum proportions
    in fmap (/ totals) proportions
