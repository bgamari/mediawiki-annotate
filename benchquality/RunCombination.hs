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
import qualified Data.Set as S
import qualified Data.Map.Lazy as ML
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector.Indexed as VI
import qualified Data.Vector.Unboxed as VU
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


import EdgeDocCorpus
import qualified CAR.KnowledgeBase as KB
import CAR.Utils
import GraphExpansionExperiments hiding (Bm25, Ql, pagesToQueryDocs)
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


import Debug.Trace

type NumResults = Int

data QuerySource = QueriesFromCbor FilePath QueryDerivation
                 | QueriesFromJson FilePath

data ModelSource = ModelFromFile FilePath -- filename to read model from
                 | TrainModel FilePath -- filename to write resulting file to
  deriving (Show)


data FeatureInfo = FeatureInfo { entFSpace       :: !(FeatureSpace EntityFeature)
                               , allEntityRunsF  :: ![Run]
                               }
                  deriving (Show)



data GridRun = GridRun T.Text
         deriving (Show, Read, Ord, Eq, Generic, Serialise, Hashable)

data Run = GridRun' GridRun | Aggr
         deriving (Show, Read, Ord, Eq, Generic, Serialise)


opts :: Parser ( FilePath
               , QuerySource
               , [CarRun.QueryId]
               , NumResults
               , FilePath
               , ModelSource
               , [FilePath]
               )
opts =
    (,,,,,,)
    <$> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")
    <*> querySource
    <*> many (option (CarRun.QueryId . T.pack <$> str) (long "query" <> metavar "QUERY" <> help "execute only this query"))
    <*> option auto (short 'k' <> long "num-results" <> help "number of results per query")
    <*> (option str (long "qrel" <> metavar "QRel-FILE" <> help "qrel file for training/testing"))
    <*> modelSource
    <*> many (option str (long "run" <> metavar "RUN" <> help "run files"))
    where

      querySource :: Parser QuerySource
      querySource =
              fromCborTitle
          <|> option (fmap QueriesFromJson str) (short 'j' <> long "queries-json" <> metavar "JSON" <> help "Queries from JSON")
        where
          queryDeriv =
              flag QueryFromPageTitle QueryFromSectionPaths
                   (long "query-from-sections" <> help "Use sections as query documents")
          fromCborTitle =
              QueriesFromCbor
                <$> option str (short 'q' <> long "queries" <> metavar "CBOR" <> help "Queries from CBOR pages")
                <*> queryDeriv

      modelSource :: Parser ModelSource
      modelSource =
            option (TrainModel <$> str) (long "train-model" <> metavar "Model-FILE" <> help "train learning-to-rank model and write to Model-FILE")
        <|> option (ModelFromFile <$> str) (long "read-model" <> metavar "Model-FILE" <> help "read learning-to-rank model from Model-FILE")


computeRankingsForQuery :: (Index.RetrievalModel Term EdgeDoc Int -> RetrievalFunction EdgeDoc)
                        -> AnnotationsFile
                        -> CarRun.QueryId  ->  [Term]
                        -> [(RetrievalFun, [(ParagraphId, Double)])]

computeRankingsForQuery
      retrieveDocs
      annsFile queryId query =
      let

        retrievalResults :: [(RetrievalFun, [(ParagraphId, Double)])]
        retrievalResults = [ (irname, retrievalResult)
                           | (irname, retrievalModel) <- retrievalModels
                           , let retrievalResult =
                                   fmap (\(ed, ld) -> (edgeDocParagraphId ed, ln ld)) $
                                   retrieveDocs retrievalModel query
                           ]

    in retrievalResults

type TrainData =  M.Map CAR.RunFile.QueryId [(QRel.DocumentName, Features, IsRelevant)]
type ReturnWithModelDiagnostics a = (a, [(String, Model, Double)])


main :: IO ()
main = do
    hSetBuffering stdout LineBuffering

    (outputFilePrefix, querySrc,
      queryRestriction, numResults
      , qrelFile, modelSource, entityRunFiles) <- execParser' 1 (helper <*> opts) mempty
    putStrLn $ "# Query restriction: " ++ show queryRestriction

    putStrLn $ "# Entity runs:  "++ (show $ fmap (show) (entityRunFiles ))

    gen0 <- newStdGen  -- needed by learning to rank
--     let siteId = "enwiki" -- todo why do we need this here?
    queries' <-
        case querySrc of
          QueriesFromCbor queryFile queryDeriv -> do
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

    entityRuns <-  mapM CAR.RunFile.readEntityRun entityRunFiles  -- mapM mapM -- first map over list
                  :: IO [[CAR.RunFile.EntityRankingEntry]]
    let entityRunsF :: [(GridRun, [CAR.RunFile.EntityRankingEntry])]
        -- entityRunsF = fmap (GridRun . CAR.RunFile.unMethodName . CAR.RunFile.carMethodName) $ fmap head $ entityRuns
        entityRunsF = [ (g, run)
                      | run <- entityRuns
                      , let g =  GridRun $ CAR.RunFile.unMethodName $ CAR.RunFile.carMethodName $ head run
                      ]

        allEntityRunsF :: [Run]
        allEntityRunsF = (fmap (GridRun' . fst) entityRunsF) <> [Aggr]

        collapsedEntityRun :: M.Map QueryId [MultiRankingEntry PageId GridRun]
        collapsedEntityRun = collapseRuns entityRunsF

        allEntityFeatures :: [EntityFeature]
        allEntityFeatures =
            (EntRetrievalFeature <$> allEntityRunsF <*> allRunFeatures)

        entFSpace :: FeatureSpace EntityFeature
        entFSpace = mkFeatureSpace allEntityFeatures

        entFInfo = FeatureInfo entFSpace allEntityRunsF



    -- predict mode
    -- alternative: load model from disk, then use graph feature vectors to produce a graph with edge weights (Graph PageId Double)
    -- use pagerank on this graph to predict an alternative node ranking
    -- save predicted node ranking as run-file
    case modelSource of
      TrainModel modelFile -> do
          let docFeatures = fmap featureVecToFeatures
                          $ makeStackedFeatures collapsedEntityRun' entFInfo
                             where collapsedEntityRun' =
                                      if null queryRestriction
                                          then collapsedEntityRun
                                          else (M.restrictKeys collapsedEntityRun (S.fromList queryRestriction))
--               docFeatures' = fmap (Features . F.toVector) docFeatures''
--               normalizer = zNormalizer $ M.elems docFeatures'
--               docFeatures = fmap (normFeatures normalizer) docFeatures'

              featureNames :: [FeatureName]
              featureNames = fmap (FeatureName . T.pack . show) $ F.featureNames entFSpace

              franking :: TrainData
              franking = augmentWithQrels qrel docFeatures Relevant

              -- Option a) drop in an svmligh style features annsFile
              -- Option b) stick into learning to rank
              discardUntrainable :: TrainData -> TrainData
              discardUntrainable franking =
                  M.filter hasPosAndNeg  franking
                where hasPosAndNeg list =
                        let hasPos = any (\(_,_,r) -> r == Relevant) list
                            hasNeg = any (\(_,_,r) -> r /= Relevant) list
                        in hasPos && hasNeg

              trainData :: TrainData
              trainData = discardUntrainable franking
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


          let trainProcedure:: String -> TrainData -> ReturnWithModelDiagnostics (Model, Double)
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
                      storeRankingData outputFilePrefix franking metric model modelDesc

              modelDiag :: [(String, Model, Double)]
              ((model, trainScore), modelDiag) = trainProcedure "train" trainData

                                -- todo load external folds
          let folds = chunksOf foldLen $ M.keys trainData
                      where foldLen = ((M.size trainData) `div` 5 ) +1

--         trainProcedure trainData = learnToRank trainData featureNames metric gen0
              (predictRanking, modelDiag') = kFoldCross trainProcedure folds trainData franking
              testScore = metric predictRanking

          -- evaluate all work here!
          mapConcurrently_ outputDiagnostics $ modelDiag ++ modelDiag'

                -- eval and write train ranking (on all data)
          storeModelData outputFilePrefix modelFile model trainScore "train"
          storeRankingData outputFilePrefix franking metric model "train"

          putStrLn $ "K-fold cross validation score " ++ (show testScore)++"."
          -- write test ranking that results from k-fold cv
          CAR.RunFile.writeEntityRun (outputFilePrefix++"-test.run")
              $ l2rRankingToRankEntries (CAR.RunFile.MethodName "l2r test")
              $ predictRanking

pagesToQueryDocs :: QueryDerivation
                 -> [Page]
                 -> [QueryDoc]
pagesToQueryDocs deriv pages =
    queryDocs
  where
    queryDocs = case deriv of
      QueryFromPageTitle ->
          [ QueryDoc { queryDocQueryId      = CarRun.pageIdToQueryId $ KB.kbDocPageId kbDoc
                     , queryDocPageId       = KB.kbDocPageId kbDoc
                     , queryDocQueryText    = getPageName $ pageName page
                     , queryDocLeadEntities = mempty
                     }
          | page <- pages
          , let kbDoc = KB.pageToKbDoc page
          ]
      QueryFromSectionPaths ->
          [ QueryDoc { queryDocQueryId      = CarRun.sectionPathToQueryId sectionPath -- KB.kbDocPageId kbDoc
                     , queryDocPageId       = pageId page
                     , queryDocQueryText    = T.unwords
                                            $ getPageName (pageName page) : getPageName (pageName page) -- twice factor
                                              : map getSectionHeading headings
                     , queryDocLeadEntities = mempty
                     }
          | page <- pages
          , let kbDoc = KB.pageToKbDoc page
          , (sectionPath, headings, _) <- pageSections page
          ]



qrelDocNameToPageId :: QRel.DocumentName -> PageId
qrelDocNameToPageId docname = packPageId $ T.unpack docname

makeStackedFeatures ::  M.Map QueryId [MultiRankingEntry PageId GridRun]
                    ->  FeatureInfo
                    ->  M.Map (QueryId, QRel.DocumentName) EntityFeatureVec
makeStackedFeatures  collapsedEntityRun entFInfo =
    let
        docFeatures''' :: M.Map (QueryId, QRel.DocumentName) EntityFeatureVec
        docFeatures''' = M.fromList
                    [ ((query, T.pack $ unpackPageId pid), features)
                    | (query, entityRun) <- M.toList collapsedEntityRun
                    , (pid, features) <- HM.toList $ generateNodeFeatures entFInfo query  entityRun
                    ]

        docFeatures' = fmap (Features . F.toVector) docFeatures'''
        normalizer = zNormalizer $ M.elems docFeatures'
        docFeatures = fmap (normFeatures normalizer) docFeatures'

    in fmap featuresToFeatureVec docFeatures



-- -------------------------------------------
--   Learning to Rank, k-fold Cross, restarts
-- -------------------------------------------

l2rRankingToRankEntries :: CAR.RunFile.MethodName -> Rankings rel CAR.RunFile.QueryId QRel.DocumentName -> [CAR.RunFile.EntityRankingEntry]
l2rRankingToRankEntries methodName rankings =
  [ CAR.RunFile.RankingEntry { carQueryId = query
                , carDocument = packPageId $ T.unpack doc
                , carRank = rank
                , carScore = score
               , carMethodName = methodName
               }
  | (query, Ranking ranking) <- M.toList rankings
  , ((score, (doc, rel)), rank) <- ranking `zip` [1..]
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
storeRankingData outputFilePrefix franking metric model modelDesc = do

  let rerankedFranking = rerankRankings' model franking
  putStrLn $ "Model "++modelDesc++" test metric "++ show (metric rerankedFranking) ++ " MAP."
  CAR.RunFile.writeEntityRun (outputFilePrefix++"-model-"++modelDesc++".run")
       $ l2rRankingToRankEntries (CAR.RunFile.MethodName $ T.pack $ "l2r "++modelDesc)
       $ rerankedFranking

kFoldCross :: forall q docId rel. (Ord q, Show q)
           => (String -> M.Map q [(docId, Features, rel)] -> ReturnWithModelDiagnostics (Model, Double))
           -> [[q]]
           -> M.Map q [(docId, Features, rel)]
           -> M.Map q [(docId, Features, rel)]
            -- -> ML.Map q (Ranking (docId, rel))
           -> ReturnWithModelDiagnostics (ML.Map q (Ranking (docId, rel)))
kFoldCross trainProcedure folds allTrainData allTestData =
    let (result, modelDiag) = unzip $ fmap (\(fidx, queries) -> trainSingleFold fidx queries) $ zip [0 .. ] folds
    in (M.unions result, concat modelDiag)
  where
    trainSingleFold :: Int -> [q]  -> ReturnWithModelDiagnostics (M.Map q (Ranking (docId, rel)))
    trainSingleFold foldIdx testQueries =
      let testData :: M.Map q [(docId, Features, rel)]
          testData =  M.filterWithKey (\query _ -> query `elem` testQueries) allTestData
          trainData :: M.Map q [(docId, Features, rel)]
          trainData =  M.filterWithKey (\query _ -> query `notElem` testQueries) allTrainData

          foldId = show foldIdx
          ((model, trainScore), modelDiag) = trainProcedure ("fold-"++foldId) trainData
          testRanking :: M.Map q (Ranking (docId, rel))
          testRanking = rerankRankings' model testData
      in (testRanking, modelDiag)

changeKey :: Ord k' => (k-> k') -> M.Map k v -> M.Map k' v
changeKey f map_ =
    M.fromList $ fmap (\(key,val) -> (f key, val)) $ M.toList map_



-- -------------------------------------------
-- the feature space
-- -------------------------------------------


data RunFeature = ScoreF
         deriving (Show, Read, Ord, Eq, Enum, Bounded, Generic, Serialise)

allRunFeatures :: [RunFeature]
allRunFeatures = [ScoreF] --[minBound..maxBound]

data EntityFeature where
    EntRetrievalFeature :: Run -> RunFeature -> EntityFeature
    deriving (Show, Read, Ord, Eq)




-- -------------------------------------------
-- make feature vectors with defaults and stuff
-- -------------------------------------------

type EntityFeatureVec = FeatureVec EntityFeature Double

makeEntFeatVector :: FeatureInfo -> [(EntityFeature, Double)] -> F.FeatureVec EntityFeature Double
makeEntFeatVector entFInfo xs =
    F.modify entFSpace' defaults xs
  where defaults = F.fromList entFSpace' ( [ feat
                                          | entityRun <- allEntityRunsF'
                                          , feat <- defaultEntRankFeatures entityRun
                                          ])
        entFSpace' = entFSpace entFInfo
        allEntityRunsF' = allEntityRunsF entFInfo

defaultRankFeatures :: RunFeature -> Double
defaultRankFeatures runF =
    case runF of
      ScoreF -> -1000.0

defaultEntRankFeatures :: Run -> [(EntityFeature, Double)]
defaultEntRankFeatures run =
    [ (EntRetrievalFeature run runF, defaultRankFeatures runF)
    | runF <- allRunFeatures
    ]

rankFeatures :: RunFeature -> RankingEntry d -> Double
rankFeatures runF entry =
    case runF of
      ScoreF -> score entry
  where
    score :: RankingEntry d -> Double
    score entry  = CAR.RunFile.carScore entry

rankEntFeatures :: Run -> RankingEntry d -> [(EntityFeature, Double)]
rankEntFeatures run entry =
    [ (EntRetrievalFeature run runF, rankFeatures runF entry)
    | runF <- allRunFeatures
    ]
-- rankEntFeatures :: FeatureSpace EntityFeature -> Run -> RankingEntry d -> [(EntityFeature, Double)]
-- rankEntFeatures entFSpace run entry =
--     [ (EntRetrievalFeature run runF, rankFeatures runF entry)
--     | runF <- allRunFeatures
--     ]


generateNodeFeatures :: FeatureInfo -> QueryId -> [MultiRankingEntry PageId GridRun] -> HM.HashMap PageId EntityFeatureVec
generateNodeFeatures entFInfo query entityRun =
     HM.fromList [ (entity, (entityScoreVec entFInfo entityRankEntry))
                  | entityRankEntry <- entityRun
                  , let entity = multiRankingEntryGetDocumentName entityRankEntry  -- for each entity in ranking...
                  ]



type Candidates = [MultiRankingEntry PageId GridRun]

selectCandidateGraph
    :: QueryId
    -> [MultiRankingEntry PageId GridRun]
    -> Candidates
selectCandidateGraph _queryId entityRun =
     entityRun

entityScoreVec :: FeatureInfo -> MultiRankingEntry PageId GridRun -> EntityFeatureVec
entityScoreVec entFInfo entityRankEntry = makeEntFeatVector  entFInfo (
       rankEntFeatures  Aggr (multiRankingEntryCollapsed entityRankEntry)
      ++ concat [ rankEntFeatures (GridRun' g) entry
         | (g, entry) <- multiRankingEntryAll entityRankEntry
         ]
     )


textToTokens :: T.Text -> Retrieve.TermCounts
textToTokens = foldMap Retrieve.oneTerm . Retrieve.textToTokens'


dropUnjudged :: Ord q
             => M.Map q [(QRel.DocumentName, Features, Maybe IsRelevant)]
             -> M.Map q [(QRel.DocumentName, Features, IsRelevant)]
dropUnjudged featureMap =
    M.filter (not . null)   -- drop entries with empty lists
    $ M.map (mapMaybe dropUnjudged') featureMap
   where dropUnjudged' (doc, feat, Just rel) = Just $ (doc, feat, rel)
         dropUnjudged' (_ , _, Nothing) = Nothing


-- -----------------------------------
-- iterative optimization
-- -----------------------------------

-- | Quite unsafe
featureVecToFeatures :: FeatureVec a Double -> Features
featureVecToFeatures = Features . F.getFeatureVec

-- | Quite unsafe
featuresToFeatureVec :: Features -> FeatureVec a Double
featuresToFeatureVec = F.unsafeFeatureVecFromVector . getFeatures

modelToFeatureVec :: (Show a, Read a, Ord a)
                  => FeatureSpace a -> Model -> FeatureVec a Double
modelToFeatureVec fspace model =
    F.fromList fspace
    [ (k', v)
    | (k, v) <- M.toList $ modelWeights model
    , let k' = read $ T.unpack $ getFeatureName k
    ]
