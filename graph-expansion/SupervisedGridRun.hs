{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

import Control.Concurrent.Async
import Data.Ord
import Data.Tuple
import Data.Semigroup hiding (All, Any, option)
import Options.Applicative
import System.IO
import Data.Aeson
import Numeric.Log
import System.Random
import GHC.Generics
import Codec.Serialise

import qualified Data.Map.Strict as M
import qualified Data.Map.Lazy as ML
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector.Indexed as VI
import qualified Data.Vector.Unboxed as VU
import qualified Text.PrettyPrint.Leijen.Text as PP
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Foldable as Foldable
import Data.Function
import Data.Bifunctor
import Data.Hashable


import CAR.Types hiding (Entity)
import CAR.ToolVersion
import CAR.AnnotationsFile as AnnsFile
import CAR.Retrieve as Retrieve
import qualified CAR.RunFile as CarRun
import CAR.TocFile as Toc
import CAR.Utils
import GridFeatures


import EdgeDocCorpus
-- import GraphExpansionExperiments hiding (Bm25, Ql)
import GraphExpansion hiding (RetrievalFun, Bm25, Ql)
import qualified SimplIR.SimpleIndex as Index
import SimplIR.LearningToRank
import SimplIR.LearningToRankWrapper
import qualified SimplIR.FeatureSpace as F
import SimplIR.FeatureSpace (featureDimension, FeatureSpace, FeatureVec, featureNames, mkFeatureSpace, concatSpace, concatFeatureVec)


import qualified CAR.RunFile as CAR.RunFile
import qualified SimplIR.Format.QRel as QRel
import qualified SimplIR.Ranking as Ranking
import MultiTrecRunFile
import PageRank
import DenseMapping
import Graph

import qualified Data.GraphViz as Dot
import qualified Data.GraphViz.Printing as Dot
import qualified Data.GraphViz.Attributes.Complete as Dot
import qualified Data.GraphViz.Commands.IO as Dot
import Control.Monad


import Debug.Trace

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
                 | TrainModel FilePath -- filename to write resulting file to
  deriving (Show)

data ExperimentSettings = AllExp | NoEdgeFeats | NoEntityFeats | AllEdgeWeightsOne | JustAggr | JustScore | JustRecip
  deriving (Show, Read, Ord, Eq, Enum, Bounded)




-- | PageRank teleportation \(\alpha\)
type TeleportationProb = Double

opts :: Parser ( FilePath
               , FilePath
               , QuerySource
               , [CarRun.QueryId]
               , NumResults
               , [(GridRun, EntityOrEdge, FilePath)]
--                , Toc.IndexedCborPath ParagraphId EdgeDoc
               , FilePath
               , ModelSource
--                , PosifyEdgeWeights
--                , TeleportationProb
               , [ExperimentSettings]
--                , PageRankExperimentSettings
--                , PageRankConvergence
--                , GraphWalkModel
               )
opts =
    (,,,,,,,,)
    <$> argument str (help "articles file" <> metavar "ANNOTATIONS-FILE")
    <*> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")
    <*> querySource
    <*> many (option (CarRun.QueryId . T.pack <$> str) (long "query" <> metavar "QUERY" <> help "execute only this query"))
    <*> option auto (short 'k' <> long "num-results" <> help "number of results per query")
    <*> some gridRunParser
--     <*> (option (Toc.IndexedCborPath <$> str)  ( long "edge-doc-cbor" <> metavar "EdgeDoc-CBOR" <> help "EdgeDoc cbor file"))
    <*> (option str (long "qrel" <> metavar "QRel-FILE"))
    <*> modelSource
--     <*> option auto (long "posify" <> metavar "OPT" <> help ("Option for how to ensure positive edge weights. Choices: " ++(show [minBound @PosifyEdgeWeights .. maxBound])) <> value Exponentiate)
--     <*> option auto (long "teleport" <> help "teleport probability (for page rank)" <> value 0.1)
    <*> many (option auto (long "exp" <> metavar "EXP" <> help ("one or more switches for experimentation. Choices: " ++(show [minBound @ExperimentSettings .. maxBound]))))
--     <*> option auto (long "pagerank-settings" <> metavar "PREXP" <> help ("Option for how to ensure positive edge weights. Choices: " ++(show [PageRankNormal,PageRankJustStructure,  PageRankWeightOffset1, PageRankWeightOffset01])) <> value PageRankNormal)
--     <*> option auto (long "pagerank-convergence" <> metavar "CONV" <> help ("How pagerank determines convergence. Choices: " ++(show [minBound @PageRankConvergence .. maxBound])) <> value Iteration10)
--     <*> option auto (long "graph-walk-model" <> metavar "PAGERANK" <> help ("Graph walk model. Choices: " ++(show [minBound @GraphWalkModel .. maxBound])) <> value PageRankWalk)
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
--         <|> option (ModelFromFile <$> str) (long "read-model" <> metavar "Model-FILE" <> help "read learning-to-rank model from Model-FILE")



-- bm25MethodName :: CarRun.MethodName
-- bm25MethodName = CarRun.MethodName "BM25"
-- qlMethodName :: CarRun.MethodName
-- qlMethodName = CarRun.MethodName "QL"


-- --------------------------------- Query Doc ------------------------------------------------------


data QueryDoc = QueryDoc { queryDocQueryId      :: !CarRun.QueryId
                         , queryDocQueryText    :: !T.Text
                         }
           deriving (Show, Generic)
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



main :: IO ()
main = do
    hSetBuffering stdout LineBuffering

    (articlesFile, outputFilePrefix, querySrc,
      queryRestriction, numResults, gridRunFiles
      , qrelFile, modelSource
      , experimentSettings
      ) <- execParser' 1 (helper <*> opts) mempty
    putStrLn $ "# Pages: " ++ show articlesFile
    putStrLn $ "# Query restriction: " ++ show queryRestriction

    let entityRunFiles  = [ (g, r) | (g, Entity, r) <- gridRunFiles]

    putStrLn $ "# Entity runs:  "++ (show $ fmap (show) (entityRunFiles ))


    queries' <-
        case querySrc of
          QueriesFromCbor queryFile queryDeriv _seedDeriv -> do
              pagesToQueryDocs queryDeriv <$> readPagesOrOutlinesAsPages queryFile

          QueriesFromJson queryFile -> do
              QueryDocList queries <- either error id . Data.Aeson.eitherDecode <$> BSL.readFile queryFile
              return queries


    let fixQRel (QRel.Entry qid docId rel) = QRel.Entry (CAR.RunFile.QueryId qid) docId rel
    qrel <- map fixQRel <$> QRel.readQRel @IsRelevant qrelFile

    queries <-
        if null queryRestriction
          then return queries'
          else do putStrLn $ "# using only queries "<>show queryRestriction
                  return $ filter (\q-> queryDocQueryId q `elem` queryRestriction) queries'
    putStrLn $ "# query count: " ++ show (length queries)



    entityRuns <-  mapM (mapM CAR.RunFile.readEntityRun) entityRunFiles  -- mapM mapM -- first map over list, then map of the snd of a tuple

    let collapsedEntityRun :: M.Map QueryId [MultiRankingEntry PageId GridRun]
        collapsedEntityRun = collapseRuns entityRuns

        tr x = traceShow x x

    case modelSource of

      TrainModel modelFile -> do
          let docFeatures = fmap featureVecToFeatures
                          $ makeFeatures collapsedEntityRun


              evalData :: TrainData
              evalData = augmentWithQrels qrel docFeatures Relevant

              -- Option b) stick into learning to rank
              discardUntrainable :: TrainData -> TrainData
              discardUntrainable evalData =
                  M.filter hasPosAndNeg  evalData
                where hasPosAndNeg list =
                        let hasPos = any (\(_,_,r) -> r == Relevant) list
                            hasNeg = any (\(_,_,r) -> r /= Relevant) list
                        in hasPos && hasNeg

              trainData :: TrainData
              trainData = discardUntrainable evalData
              metric = avgMetricQrel qrel
              totalElems = getSum . foldMap ( Sum . length ) $ trainData
              totalPos = getSum . foldMap ( Sum . length . filter (\(_,_,rel) -> rel == Relevant)) $ trainData

          putStrLn $ "Training model with (trainData) "++ show (M.size trainData) ++
                    " queries and "++ show totalElems ++" items total of which "++
                    show totalPos ++" are positive."

          let displayTrainData :: TrainData
                               -> [String]
              displayTrainData trainData =
                [ show k ++ " -> "++ show elem
                | (k,list) <- M.toList trainData
                , elem <- list]

          putStrLn $ "Training Data = \n" ++ intercalate "\n" (take 10 $ displayTrainData trainData)
          trainMe trainData evalData entFSpace metric outputFilePrefix modelFile

trainMe :: TrainData -> TrainData -> _ -> _ -> FilePath -> FilePath -> IO ()
trainMe trainData evalData entFSpace metric outputFilePrefix modelFile = do
          -- train me!
          gen0 <- newStdGen  -- needed by learning to rank
          let
              featureNames :: _
              featureNames = fmap (FeatureName . T.pack . show) $ F.featureNames entFSpace

              trainProcedure:: String -> TrainData -> ReturnWithModelDiagnostics (Model, Double)
              trainProcedure modelDesc trainData =
                  let trainWithDifferentGens :: StdGen -> Int -> (StdGen, ReturnWithModelDiagnostics (Model, Double))
                      trainWithDifferentGens gen i =
                                let (genA, genB) = System.Random.split gen
                                in (genA, restartModel i genB)
                        where restartModel i gen =
                                let (model, trainScore) = learnToRank trainData featureNames metric gen
                                in ((model, trainScore), [((modelDesc ++ "-restart-"++show i), model, trainScore)])


                      other:: [ReturnWithModelDiagnostics (Model, Double)]
                      (_, other) = mapAccumL trainWithDifferentGens gen0 [0..4]
                      modelsWithTrainScore:: [(Model, Double)]
                      (modelsWithTrainScore, modelDiag) = unzip other
                      (model, trainScore) = maximumBy (compare `on` snd) modelsWithTrainScore
                  in ((model, trainScore), [(modelDesc ++"-best", model, trainScore)] ++ concat modelDiag)


          let outputDiagnostics :: (String, Model, Double) -> IO ()
              outputDiagnostics (modelDesc,model, trainScore) = do
                      storeModelData outputFilePrefix modelFile model trainScore modelDesc
                      storeRankingData outputFilePrefix evalData metric model modelDesc

              modelDiag :: [(String, Model, Double)]
              ((model, trainScore), modelDiag) = trainProcedure "train" trainData

            -- todo  exportGraphs model

                                -- todo load external folds
          let folds = chunksOf foldLen $ M.keys trainData
                      where foldLen = ((M.size trainData) `div` 5 ) +1

--         trainProcedure trainData = learnToRank trainData featureNames metric gen0
              (predictRanking, modelDiag') = kFoldCross trainProcedure folds trainData evalData
              testScore = metric predictRanking

          -- evaluate all work here!
          mapConcurrently_ outputDiagnostics $ modelDiag ++ modelDiag'

                -- eval and write train ranking (on all data)
          storeModelData outputFilePrefix modelFile model trainScore "train"
          storeRankingData outputFilePrefix evalData metric model "train"

          putStrLn $ "K-fold cross validation score " ++ (show testScore)++"."
          -- write test ranking that results from k-fold cv
          CAR.RunFile.writeEntityRun (outputFilePrefix++"-test.run")
              $ l2rRankingToRankEntries (CAR.RunFile.MethodName "l2r test")
              $ predictRanking


l2rRankingToRankEntries :: CAR.RunFile.MethodName
                        -> M.Map CAR.RunFile.QueryId (Ranking SimplIR.LearningToRank.Score (QRel.DocumentName, rel))
                        -> [CAR.RunFile.EntityRankingEntry]
l2rRankingToRankEntries methodName rankings =
  [ CAR.RunFile.RankingEntry { carQueryId = query
                             , carDocument = packPageId $ T.unpack doc
                             , carRank = rank
                             , carScore = score
                             , carMethodName = methodName
                             }
  | (query, ranking) <- M.toList rankings
  , ((score, (doc, rel)), rank) <- Ranking.toSortedList ranking `zip` [1..]
  ]


-- Train model on all data
storeModelData :: FilePath
               -> FilePath
               -> Model
               -> Double
               -> [Char]
               -> IO ()
storeModelData outputFilePrefix modelFile model trainScore modelDesc = do
  putStrLn $ "Model "++modelDesc++ " train metric "++ (show trainScore) ++ " MAP."
  let modelFile' = outputFilePrefix++modelFile++"-model-"++modelDesc++".json"
  BSL.writeFile modelFile' $ Data.Aeson.encode model
  putStrLn $ "Written model "++modelDesc++ " to file "++ (show modelFile') ++ " ."

storeRankingData ::  FilePath
               -> TrainData
               -> ScoringMetric IsRelevant CAR.RunFile.QueryId QRel.DocumentName
               -> Model
               -> [Char]
               -> IO ()
storeRankingData outputFilePrefix evalData metric model modelDesc = do

  let rerankedFranking = rerankRankings' model evalData
  putStrLn $ "Model "++modelDesc++" test metric "++ show (metric rerankedFranking) ++ "MAP."
  CAR.RunFile.writeEntityRun (outputFilePrefix++"-model-"++modelDesc++".run")
       $ l2rRankingToRankEntries (CAR.RunFile.MethodName $ T.pack $ "l2r "++modelDesc)
       $ rerankedFranking

kFoldCross :: forall q docId rel. (Ord q, Show q)
           => (String -> M.Map q [(docId, Features, rel)] -> ReturnWithModelDiagnostics (Model, Double))
           -> [[q]]
           -> M.Map q [(docId, Features, rel)]
           -> M.Map q [(docId, Features, rel)]
            -- -> ML.Map q (Ranking (docId, rel))
           -> ReturnWithModelDiagnostics (ML.Map q (Ranking SimplIR.LearningToRank.Score (docId, rel)))
kFoldCross trainProcedure folds allTrainData allTestData =
    let (result, modelDiag) = unzip $ fmap (\(fidx, queries) -> trainSingleFold fidx queries) $ zip [0 .. ] folds
    in (M.unions result, concat modelDiag)
  where
    trainSingleFold :: Int -> [q]  -> ReturnWithModelDiagnostics (M.Map q (Ranking SimplIR.LearningToRank.Score (docId, rel)))
    trainSingleFold foldIdx testQueries =
      let testData :: M.Map q [(docId, Features, rel)]
          testData =  M.filterWithKey (\query _ -> query `elem` testQueries) allTestData
          trainData :: M.Map q [(docId, Features, rel)]
          trainData =  M.filterWithKey (\query _ -> query `notElem` testQueries) allTrainData

          foldId = show foldIdx
          ((model, trainScore), modelDiag) = trainProcedure ("fold-"++foldId) trainData
          testRanking :: M.Map q (Ranking SimplIR.LearningToRank.Score (docId, rel))
          testRanking = rerankRankings' model testData
      in (testRanking, modelDiag)


--
-- expSettingToCrit :: [ExperimentSettings] ->  (CombinedFeature -> Bool)
-- expSettingToCrit exps fname =
--     all (`convert` fname) exps
--   where
--     convert :: ExperimentSettings -> (CombinedFeature -> Bool)
--     convert exp = case exp of
--                     AllExp -> const True
--                     NoEdgeFeats -> noEdge
--                     NoEntityFeats -> noEntity
--                     AllEdgeWeightsOne -> const True -- needs to be handled elsewhere
--                     JustAggr -> onlyAggr
--                     JustScore -> onlyScore
--                     JustRecip -> onlyRR


makeFeatures :: M.Map QueryId [MultiRankingEntry PageId GridRun] -> M.Map (QueryId, QRel.DocumentName) EntityFeatureVec
makeFeatures collapsedEntityRun =
    let
        docFeatures''' :: M.Map (QueryId, QRel.DocumentName) EntityFeatureVec
        docFeatures''' = M.fromList
                    [ ((query, T.pack $ unpackPageId pid), features)
                    | (query, entityRun) <- M.toList collapsedEntityRun
--                     , let entityRun = fromMaybe [] $ query `M.lookup` collapsedEntityRun
                    , entityRankEntry <- entityRun
                    , let pid = multiRankingEntryGetDocumentName entityRankEntry
                    , let features =  makeEntFeatVector (entityScoreVecFromMultiRankings entityRankEntry)
--                     , --let candidates = selectCandidateGraph edgeDocsLookup query edgeRun entityRun
--                     , --((qid, pid), features) <- HM.toList $ combineEntityEdgeFeatures query candidates
                    ]


     -- todo filter with expCrits
--         docFeatures'' = fmap crit docFeatures'''
--                         where crit = filterExpSettings combinedFSpace combinedFSpace' (expSettingToCrit experimentSettings)

        docFeatures' = fmap (Features . F.toVector) docFeatures'''
        normalizer = zNormalizer $ M.elems docFeatures'
        docFeatures = fmap (normFeatures normalizer) docFeatures'

    in fmap featuresToFeatureVec docFeatures

