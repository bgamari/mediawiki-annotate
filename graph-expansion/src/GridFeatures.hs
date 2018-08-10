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
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

module GridFeatures where

import Options.Applicative
import Data.Aeson
import Numeric.Log
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
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Foldable as Foldable
import Data.Function
import Data.Bifunctor
import Data.Hashable
import Data.Semigroup hiding (option)
import System.Random
import Control.Concurrent.Async

import CAR.Types hiding (Entity)
import CAR.ToolVersion
import CAR.AnnotationsFile as AnnsFile
import CAR.Retrieve as Retrieve
import qualified CAR.RunFile as CarRun
import CAR.Utils


import qualified SimplIR.SimpleIndex as Index
import SimplIR.LearningToRank
import SimplIR.LearningToRankWrapper
import qualified SimplIR.FeatureSpace as F
import SimplIR.FeatureSpace (featureDimension, FeatureSpace, FeatureVec, featureNames, mkFeatureSpace, concatSpace, concatFeatureVec)


import qualified CAR.RunFile as CAR.RunFile
import qualified SimplIR.Format.QRel as QRel
import qualified SimplIR.Ranking as Ranking
import MultiTrecRunFile





-- GridRun  QueryModel RetrievalModel ExpansionModel IndexType
gridRunParser :: Parser (GridRun, EntityOrEdge, FilePath)
gridRunParser = option (str >>= parseGridRunFile) (long "grid-run")
  where
    parseGridRunFile :: String -> ReadM (GridRun, EntityOrEdge, FilePath)
    parseGridRunFile s
      | a:b:c:d:e:rest <- words s
      = do !a' <- safeRead "QueryModel" a
           !b' <- safeRead "RetrievalModel" b
           !c' <- safeRead "ExpansionModel" c
           !d' <- safeRead "IndexType" d
           !e' <- safeRead "EntityOrEdge" e
           return (GridRun a' b' c' d', e', unwords rest)
      | otherwise
      = fail $ "Failed to tokenise: " ++ s
      where
        safeRead :: Read a => String -> String -> ReadM a
        safeRead thing s'
          | (x,""):_ <- reads s' = return x
          | otherwise = fail $ "failed to parse "++thing++": "++s'


-- -------------------------------------------
-- the feature space
-- -------------------------------------------

data QueryModel = All | Title
         deriving (Show, Read, Ord, Eq, Enum, Bounded, Generic, Serialise, Hashable)
data RetrievalModel = Bm25 | Ql
         deriving (Show, Read, Ord, Eq, Enum, Bounded, Generic, Serialise, Hashable)
data ExpansionModel = NoneX | Rm | EcmX | EcmRm | EcmPsg
         deriving (Show, Read, Ord, Eq, Enum, Bounded, Generic, Serialise, Hashable)
data IndexType = EcmIdx | EntityIdx | PageIdx | ParagraphIdx
         deriving (Show, Read, Ord, Eq, Enum, Bounded, Generic, Serialise, Hashable)

entityRunsF :: [GridRun]
entityRunsF = [ GridRun qm rm em it
             | qm <- [minBound..maxBound]
             , rm <- [minBound..maxBound]
             , (it, em) <- [ (EcmIdx, EcmX), (EcmIdx, EcmRm)
                           , (ParagraphIdx, EcmX), (ParagraphIdx, EcmRm)
                           -- , (PageIdx, NoneX), (PageIdx, Rm)]
                           ]
                           ++ [(EntityIdx, em) | em <- [minBound..maxBound]]
                           ++ [(PageIdx, em) | em <- [minBound..maxBound]]
             ]


edgeRunsF :: [GridRun]
edgeRunsF = [ GridRun qm rm em it
             | qm <- [minBound..maxBound]
             , rm <- [minBound..maxBound]
             , (it, em) <- [ (EcmIdx, NoneX), (EcmIdx, Rm), (EcmIdx, EcmPsg)
                           , (ParagraphIdx, NoneX), (ParagraphIdx, Rm), (ParagraphIdx, EcmPsg)
                           ]
             ]



data GridRun = GridRun QueryModel RetrievalModel ExpansionModel IndexType
         deriving (Show, Read, Ord, Eq, Generic, Serialise, Hashable)

data Run = GridRun' GridRun | Aggr
         deriving (Show, Read, Ord, Eq, Generic, Serialise)
allEntityRunsF = (GridRun' <$> entityRunsF) <> [Aggr]
allEdgeRunsF = (GridRun' <$> edgeRunsF) <> [Aggr]

data RunFeature = ScoreF | RecipRankF | CountF --LinearRankF | BucketRankF
         deriving (Show, Read, Ord, Eq, Enum, Bounded, Generic, Serialise)

allRunFeatures :: [RunFeature]
allRunFeatures = [ScoreF] --[minBound..maxBound]

data EntityFeature where
    EntRetrievalFeature :: Run -> RunFeature -> EntityFeature
    EntIncidentEdgeDocsRecip :: EntityFeature
    EntDegreeRecip :: EntityFeature
    EntDegree  :: EntityFeature
    deriving (Show, Read, Ord, Eq)

data EdgeFeature where
    EdgeRetrievalFeature :: Run -> RunFeature -> EdgeFeature
    EdgeDocKL  :: EdgeFeature
    EdgeCount  :: EdgeFeature
    deriving (Show, Read, Ord, Eq)

data EntityOrEdge = Entity | Edge
         deriving (Show, Read, Ord, Eq, Enum, Bounded, Generic, Serialise)

type CombinedFeature = Either EntityFeature EdgeFeature

allEntityFeatures :: [EntityFeature]
allEntityFeatures =
    (EntRetrievalFeature <$> allEntityRunsF <*> allRunFeatures)
    <> [EntIncidentEdgeDocsRecip, EntDegreeRecip, EntDegree]


allEdgeFeatures :: [EdgeFeature]
allEdgeFeatures =
    (EdgeRetrievalFeature <$> allEdgeRunsF <*> allRunFeatures)
    <> [EdgeDocKL, EdgeCount]

entFSpace :: FeatureSpace EntityFeature
entFSpace = mkFeatureSpace allEntityFeatures

edgeFSpace :: FeatureSpace EdgeFeature
edgeFSpace = mkFeatureSpace allEdgeFeatures

combinedFSpace :: FeatureSpace CombinedFeature
combinedFSpace = concatSpace entFSpace edgeFSpace




-- -------------------------------------------
-- filtering of feature spaces
-- -------------------------------------------


filterExpSettingsEdge ::  FeatureSpace EdgeFeature
                  ->  FeatureSpace EdgeFeature
                  -> (EdgeFeature -> Bool)
                  ->  (FeatureVec EdgeFeature Double)
                  ->  (FeatureVec EdgeFeature Double)
filterExpSettingsEdge fromFeatSpace toFeatSpace pred features =
    F.fromList toFeatSpace
    $ [ pair
      | pair@(fname, _) <- F.toList fromFeatSpace features
      , pred fname
      ]

onlyAggrEdge :: EdgeFeature -> Bool
onlyAggrEdge (EdgeRetrievalFeature Aggr runf) = True
onlyAggrEdge _  = False

onlyScoreEdge :: EdgeFeature -> Bool
onlyScoreEdge (EdgeRetrievalFeature _ ScoreF) = True
onlyScoreEdge _  = False

onlyRREdge :: EdgeFeature -> Bool
onlyRREdge (EdgeRetrievalFeature _ RecipRankF) = True
onlyRREdge _  = False



filterExpSettings ::  FeatureSpace CombinedFeature
                  ->  FeatureSpace CombinedFeature
                  -> (CombinedFeature -> Bool)
                  ->  (FeatureVec CombinedFeature Double)
                  ->  (FeatureVec CombinedFeature Double)
filterExpSettings fromFeatSpace toFeatSpace pred features =
    F.fromList toFeatSpace
    $ [ pair
      | pair@(fname, _) <- F.toList fromFeatSpace features
      , pred fname
      ]
noEntity :: CombinedFeature -> Bool
noEntity (Left _) = False
noEntity _  = True

noEdge :: CombinedFeature -> Bool
noEdge (Right _) = False
noEdge _  = True

onlyAggr :: CombinedFeature -> Bool
onlyAggr (Left (EntRetrievalFeature Aggr runf)) = True
onlyAggr (Right (EdgeRetrievalFeature Aggr runf)) = True
onlyAggr _  = False

onlyScore :: CombinedFeature -> Bool
onlyScore (Left (EntRetrievalFeature _ ScoreF)) = True
onlyScore (Right (EdgeRetrievalFeature _ ScoreF)) = True
onlyScore _  = False

onlyRR :: CombinedFeature -> Bool
onlyRR (Left (EntRetrievalFeature _ RecipRankF)) = True
onlyRR (Right (EdgeRetrievalFeature _ RecipRankF)) = True
onlyRR _  = False




-- -------------------------------------------
-- make feature vectors with defaults and stuff
-- -------------------------------------------

type EdgeFeatureVec = FeatureVec EdgeFeature Double
type EntityFeatureVec = FeatureVec EntityFeature Double
type CombinedFeatureVec = FeatureVec CombinedFeature Double




makeEntFeatVector :: [(EntityFeature, Double)] -> F.FeatureVec EntityFeature Double
makeEntFeatVector xs =
    F.modify entFSpace defaults xs
 where defaults = F.fromList entFSpace ([ (EntIncidentEdgeDocsRecip, 0.0)
                                       , (EntDegreeRecip, 0.0)
                                       , (EntDegree, 0.0)
                                       ]
                                       ++ [ feat
                                          | entityRun <- allEntityRunsF
                                          , feat <- defaultEntRankFeatures entityRun
                                          ]
                                       )

makeEdgeFeatVector :: [(EdgeFeature, Double)] -> F.FeatureVec EdgeFeature Double
makeEdgeFeatVector xs =
    F.modify edgeFSpace defaults xs
 where defaults = F.fromList edgeFSpace ([ (EdgeCount, 0.0)
                                         , (EdgeDocKL, 0.0)
                                         ]
                                        ++ [ feat
                                           | edgeRun <- allEdgeRunsF
                                           , feat <- defaultEdgeRankFeatures edgeRun
                                           ]
                                        )

defaultRankFeatures :: RunFeature -> Double
defaultRankFeatures runF =
    case runF of
      ScoreF -> -1000.0
      RecipRankF -> 0.0
--       LinearRankF -> 0.0
--       BucketRankF -> 0.0
      CountF -> 0.0

defaultEntRankFeatures :: Run -> [(EntityFeature, Double)]
defaultEntRankFeatures run =
    [ (EntRetrievalFeature run runF, defaultRankFeatures runF)
    | runF <- allRunFeatures
    ]

defaultEdgeRankFeatures :: Run -> [(EdgeFeature, Double)]
defaultEdgeRankFeatures run =
    [ (EdgeRetrievalFeature run runF, defaultRankFeatures runF)
    | runF <- allRunFeatures
    ]

rankFeatures :: RunFeature -> RankingEntry d -> Double
rankFeatures runF entry =
    case runF of
      ScoreF -> score entry
      RecipRankF -> recipRank entry
--       LinearRankF -> linearRank 100  entry
--       BucketRankF -> bucketRank entry
      CountF -> count entry
  where
    score :: RankingEntry d -> Double
    score entry  = CAR.RunFile.carScore entry

    recipRank :: RankingEntry d  -> Double
    recipRank entry = 1.0/ (1.0 + realToFrac rank)
      where rank = CAR.RunFile.carRank entry

    linearRank :: Int -> RankingEntry d  -> Double
    linearRank maxLen entry
        | rank > maxLen = 0.0
        | otherwise = realToFrac $ maxLen - rank
      where rank = CAR.RunFile.carRank entry

    bucketRank :: RankingEntry d  -> Double
    bucketRank entry
        | rank >= 5 = 3.0
        | rank >= 20 = 2.0
        | otherwise = 1.0
      where rank = CAR.RunFile.carRank entry

    count :: RankingEntry d -> Double
    count _ = 1.0

rankEntFeatures :: Run -> RankingEntry d -> [(EntityFeature, Double)]
rankEntFeatures run entry =
    [ (EntRetrievalFeature run runF, rankFeatures runF entry)
    | runF <- allRunFeatures
    ]

rankEdgeFeatures :: Run -> RankingEntry d -> [(EdgeFeature, Double)]
rankEdgeFeatures run entry =
    [ (EdgeRetrievalFeature run runF, rankFeatures runF entry)
    | runF <- allRunFeatures
    ]


-- ---------------------------------------------------------
-- ---------------------------------------------------------


type Q = CAR.RunFile.QueryId
type DocId = QRel.DocumentName
type Rel = IsRelevant
type TrainData =  M.Map Q [(DocId, Features, Rel)]
type ReturnWithModelDiagnostics a = (a, [(String, Model, Double)])
type FoldRestartResults = Folds (M.Map Q [(DocId, Features, Rel)], [(Model, Double)])
type BestFoldResults  = Folds (M.Map Q [(DocId, Features, Rel)], (Model, Double))


trainMe :: Show f
        => StdGen
        -> TrainData
        -> FeatureSpace f
        -> ScoringMetric IsRelevant CAR.RunFile.QueryId QRel.DocumentName
        -> FilePath
        -> FilePath
        -> IO ()
trainMe gen0  trainData featureSpace metric outputFilePrefix modelFile = do
          -- train me!
          let

              featureNames :: [FeatureName]
              featureNames = fmap (FeatureName . T.pack . show) $ F.featureNames featureSpace

              (model, trainScore) = bestModel $ trainWithRestarts gen0 metric featureNames trainData

            -- todo  exportGraphs model

                                -- todo load external folds
              folds = mkSequentialFolds 5 (M.keys trainData)

              foldRestartResults :: Folds (M.Map  Q [(DocId, Features, Rel)], [(Model, Double)])
              foldRestartResults = kFolds (trainWithRestarts gen0 metric featureNames) trainData folds

          dumpKFoldModelsAndRankings foldRestartResults metric outputFilePrefix modelFile
          dumpFullModelsAndRankings trainData (model, trainScore) metric outputFilePrefix modelFile

trainWithRestarts :: StdGen
                -> ScoringMetric IsRelevant CAR.RunFile.QueryId QRel.DocumentName
                -> [FeatureName]
                -> TrainData
                -> [(Model, Double)]
trainWithRestarts gen0 metric featureNames trainData =
  let trainData' = discardUntrainable trainData

      rngSeeds :: [StdGen]
      rngSeeds = unfoldr (Just . System.Random.split) gen0

      restartModel ::  StdGen -> (Model, Double)
      restartModel gen =
        learnToRank trainData' featureNames metric gen0

      modelsWithTrainScore :: [(Model,Double)]
      modelsWithTrainScore = fmap restartModel rngSeeds
     in modelsWithTrainScore


discardUntrainable :: TrainData -> TrainData
discardUntrainable evalData =
    M.filter hasPosAndNeg  evalData
  where
    hasPosAndNeg list =
        let hasPos = any (\(_,_,r) -> r == Relevant) list
            hasNeg = any (\(_,_,r) -> r /= Relevant) list
        in hasPos && hasNeg


bestPerFold :: FoldRestartResults -> BestFoldResults
bestPerFold = fmap (second bestModel)

bestModel ::  [(Model, Double)] -> (Model, Double)
bestModel = maximumBy (compare `on` snd)


bestRankingPerFold :: BestFoldResults -> Folds (M.Map Q (Ranking SimplIR.LearningToRank.Score (DocId, Rel)))
bestRankingPerFold bestPerFold' =
    fmap (\(testData, (model, trainScore))  ->  rerankRankings' model testData) bestPerFold'



dumpKFoldModelsAndRankings  :: FoldRestartResults
                            -> ScoringMetric IsRelevant CAR.RunFile.QueryId QRel.DocumentName
                            -> FilePath
                            -> FilePath
                            -> IO()
dumpKFoldModelsAndRankings foldRestartResults metric outputFilePrefix modelFile = do
          let bestPerFold' :: Folds (M.Map Q [(DocId, Features, Rel)], (Model, Double))
              bestPerFold' = bestPerFold foldRestartResults

              bestRankingPerFold' :: Folds (M.Map Q (Ranking SimplIR.LearningToRank.Score (DocId, Rel)))
              bestRankingPerFold' = bestRankingPerFold bestPerFold'

              testRanking ::   M.Map Q (Ranking SimplIR.LearningToRank.Score (DocId, Rel))
              testRanking = fold bestRankingPerFold'

              testScore = metric testRanking

              dumpAll = [ do storeRankingData outputFilePrefix ranking metric modelDesc
                             storeModelData outputFilePrefix modelFile model trainScore modelDesc
                        | (foldNo, (testData, restartModels))  <- zip [0..] $ toList foldRestartResults
                        , (restartNo, (model, trainScore)) <- zip [0..] restartModels
                        , let ranking = rerankRankings' model testData
                        , let modelDesc = "fold-"<> show foldNo <> "-restart-"<> show restartNo
                        ]

              dumpBest = [ do storeRankingData outputFilePrefix ranking metric modelDesc
                              storeModelData outputFilePrefix modelFile model trainScore modelDesc
                        | (foldNo, (testData,  (model, trainScore)))  <- zip [0..] $ toList bestPerFold'
                        , let ranking = rerankRankings' model testData
                        , let modelDesc = "fold-"<> show foldNo <> "-best"
                        ]

              dumpKfoldTestRanking = storeRankingData outputFilePrefix testRanking metric modelDesc
                                where modelDesc = "test"


          mapConcurrently_ id $ dumpAll ++ dumpBest ++ [dumpKfoldTestRanking]



dumpFullModelsAndRankings :: M.Map Q [(DocId, Features, Rel)]
                            -> (Model, Double)
                            -> ScoringMetric IsRelevant CAR.RunFile.QueryId QRel.DocumentName
                            -> FilePath
                            -> FilePath
                            -> IO()
dumpFullModelsAndRankings trainData (model, trainScore) metric outputFilePrefix modelFile = do
    let modelDesc = "train"
        trainRanking = rerankRankings' model trainData
    storeRankingData outputFilePrefix trainRanking metric modelDesc
    storeModelData outputFilePrefix modelFile model trainScore modelDesc

--
-- outputTrainedModelsAndRanking :: TrainData
--         -> ScoringMetric IsRelevant CAR.RunFile.QueryId QRel.DocumentName
--         -> FilePath
--         -> FilePath
--         -> IO ()
-- outputTrainedModelsAndRanking evalData metric outputFilePrefix modelFile =
--
--           let outputDiagnostics :: (String, Model, Double) -> IO ()
--               outputDiagnostics (modelDesc,model, trainScore) = do
--                       storeModelData outputFilePrefix modelFile model trainScore modelDesc
--                       storeRankingData outputFilePrefix evalData metric model modelDesc
--
--           -- evaluate all work here!
--           mapConcurrently_ outputDiagnostics $ modelDiag ++ modelDiag'
--
--                 -- eval and write train ranking (on all data)
--           storeModelData outputFilePrefix modelFile model trainScore "train"
--           storeRankingData outputFilePrefix evalData metric model "train"
--
--           putStrLn $ "K-fold cross validation score " ++ (show testScore)++"."
--           -- write test ranking that results from k-fold cv
--           CAR.RunFile.writeEntityRun (outputFilePrefix++"-test.run")
--               $ l2rRankingToRankEntries (CAR.RunFile.MethodName "l2r test")
--               $ predictRanking


l2rRankingToRankEntries :: CAR.RunFile.MethodName
                        -> M.Map CAR.RunFile.QueryId (Ranking SimplIR.LearningToRank.Score (QRel.DocumentName, Rel))
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
--                -> TrainData
               -> _
               -> ScoringMetric IsRelevant CAR.RunFile.QueryId QRel.DocumentName
               -> String
               -> IO ()
storeRankingData outputFilePrefix ranking metric modelDesc = do

--   let rerankedFranking = rerankRankings' model evalData
  putStrLn $ "Model "++modelDesc++" test metric "++ show (metric ranking) ++ "MAP."
  CAR.RunFile.writeEntityRun (outputFilePrefix++"-model-"++modelDesc++".run")
       $ l2rRankingToRankEntries (CAR.RunFile.MethodName $ T.pack $ "l2r "++modelDesc)
       $ ranking


newtype Folds a = Folds { getFolds :: [a] }
                deriving (Foldable, Functor)

mkSequentialFolds :: Int -> [a] -> Folds [a]
mkSequentialFolds k xs = Folds $ chunksOf foldLen xs
       where foldLen = (length xs `div` k) + 1


-- r might be: [(Model, Double)]


kFolds :: forall q docId rel r.
       Eq q
       => (M.Map q [(docId, Features, rel)] -> r)
       -> M.Map q [(docId, Features, rel)]
          -- ^ training data
       -> Folds [q]
       -> Folds (M.Map q [(docId, Features, rel)], r)
          -- ^ the training result and set of test data for the fold
kFolds trainWithRestarts allData foldQueries =
    fmap trainSingleFold foldQueries
  where
    trainSingleFold :: [q] -> (M.Map q [(docId, Features, rel)], r)
    trainSingleFold testQueries =
      let testData :: M.Map q [(docId, Features, rel)]
          testData =  M.filterWithKey (\query _ -> query `elem` testQueries) allData

          trainData :: M.Map q [(docId, Features, rel)]
          trainData =  M.filterWithKey (\query _ -> query `notElem` testQueries) allData
      in (testData, trainWithRestarts trainData)





-- ---------------------------------------------------------
-- ---------------------------------------------------------

entityScoreVecFromMultiRankings :: MultiRankingEntry PageId GridRun -> [(EntityFeature, Double)]
entityScoreVecFromMultiRankings entityRankEntry =
      rankEntFeatures Aggr (multiRankingEntryCollapsed entityRankEntry)
      ++ concat [ rankEntFeatures (GridRun' g) entry
         | (g, entry) <- multiRankingEntryAll entityRankEntry
         ]

-- ---------------------------------------------------------
--       Low level feature wrangling
-- ---------------------------------------------------------



-- | Quite unsafe
featureVecToFeatures :: FeatureVec a Double -> Features
featureVecToFeatures = Features . F.getFeatureVec

-- | Quite unsafe
featuresToFeatureVec :: Features -> FeatureVec a Double
featuresToFeatureVec = F.unsafeFeatureVecFromVector . getFeatures


fconcat :: Features -> Features -> Features
fconcat (Features xs) (Features ys) = Features  (xs VU.++ ys)

fsum :: Features -> Features -> Features
fsum (Features xs) (Features ys) = Features $ VU.zipWith (+) xs ys
