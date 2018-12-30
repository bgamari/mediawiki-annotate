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
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}


module TrainAndStore where

import Options.Applicative
import Data.Aeson
import GHC.Generics
import Codec.Serialise

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Data.List
import Data.List.Split
import Data.Foldable as Foldable
import Data.Function
import Data.Bifunctor
import Data.Hashable
import System.Random
import Control.Parallel.Strategies
import Control.Concurrent.Map
import Control.DeepSeq

import CAR.Types hiding (Entity)
import qualified CAR.RunFile as CarRun

import SimplIR.LearningToRank
import SimplIR.LearningToRankWrapper
import qualified SimplIR.FeatureSpace as F
import SimplIR.FeatureSpace (featureDimension, FeatureSpace, FeatureVec, featureNames, mkFeatureSpace, concatSpace, concatFeatureVec)

import qualified CAR.RunFile as CAR.RunFile
import qualified SimplIR.Format.QRel as QRel
import qualified SimplIR.Ranking as Ranking
import MultiTrecRunFile
import SimplIR.TrainUtils


type Q = CAR.RunFile.QueryId
type DocId = QRel.DocumentName
type Rel = IsRelevant
type TrainData f =  M.Map Q [(DocId, FeatureVec f Double, Rel)]
-- type ReturnWithModelDiagnostics a = (a, [(String, Model, Double)])
type FoldRestartResults f = Folds (M.Map Q [(DocId, FeatureVec f Double, Rel)],
                                   [(Model f, Double)])
type BestFoldResults f = Folds (M.Map Q [(DocId, FeatureVec f Double, Rel)], (Model f, Double))


trainMe :: forall f. (Ord f, Show f)
        => MiniBatchParams
        -> StdGen
        -> TrainData f
        -> FeatureSpace f
        -> ScoringMetric IsRelevant CAR.RunFile.QueryId QRel.DocumentName
        -> FilePath
        -> FilePath
        -> IO ()
trainMe miniBatchParams gen0 trainData fspace metric outputFilePrefix modelFile = do
          -- train me!
          let nRestarts = 5
              nFolds = 5

          -- folded CV
                                -- todo load external folds
              !folds = force $ mkSequentialFolds nFolds (M.keys trainData)
          putStrLn "made folds"

          let trainFun :: FoldIdx -> _
              trainFun foldIdx =
                  take nRestarts . trainWithRestarts miniBatchParams gen0 metric infoStr fspace
                where
                  infoStr = show foldIdx

              foldRestartResults :: Folds (M.Map  Q [(DocId, FeatureVec f Double, Rel)], [(Model f, Double)])
              foldRestartResults = kFolds trainFun trainData folds

              strat :: Strategy (Folds (a, [(Model f, Double)]))
              strat = parTraversable (evalTuple2 r0 (parTraversable rdeepseq))
          foldRestartResults' <- withStrategyIO strat foldRestartResults

          let actions1 = dumpKFoldModelsAndRankings foldRestartResults' metric outputFilePrefix modelFile

          -- full train
          let fullRestarts = withStrategy (parTraversable rdeepseq)
                             $ take nRestarts $ trainWithRestarts miniBatchParams gen0 metric "full" fspace trainData
              (model, trainScore) =  bestModel $  fullRestarts
              actions2 = dumpFullModelsAndRankings trainData (model, trainScore) metric outputFilePrefix modelFile

          mapConcurrentlyL_ 24 id $ actions1 ++ actions2
          putStrLn "dumped all models and rankings"

trainWithRestarts
    :: forall f. (Show f)
    => MiniBatchParams
    -> StdGen
    -> ScoringMetric IsRelevant CAR.RunFile.QueryId QRel.DocumentName
    -> String
    -> FeatureSpace f
    -> TrainData f
    -> [(Model f, Double)]
       -- ^ an infinite list of restarts
trainWithRestarts miniBatchParams gen0 metric info fspace trainData =
  let trainData' = discardUntrainable trainData

      rngSeeds :: [StdGen]
      rngSeeds = unfoldr (Just . System.Random.split) gen0

      restartModel :: Int -> StdGen -> (Model f, Double)
      restartModel restart =
          learnToRank miniBatchParams (defaultConvergence info' 1e-2 100 2) trainData' fspace metric
        where
          info' = info <> " restart " <> show restart
      modelsWithTrainScore :: [(Model f,Double)]
      modelsWithTrainScore = zipWith restartModel [0..] rngSeeds
     in modelsWithTrainScore



discardUntrainable :: TrainData f -> TrainData f
discardUntrainable evalData =
    M.filter hasPosAndNeg  evalData
  where
    hasPosAndNeg list =
        let hasPos = any (\(_,_,r) -> r == Relevant) list
            hasNeg = any (\(_,_,r) -> r /= Relevant) list
        in hasPos && hasNeg


bestPerFold :: FoldRestartResults f -> BestFoldResults f
bestPerFold = fmap (second bestModel)

bestModel ::  [(Model f, Double)] -> (Model f, Double)
bestModel = maximumBy (compare `on` snd)


bestRankingPerFold :: forall f . ()
                   => BestFoldResults f
                   -> Folds (M.Map Q (Ranking SimplIR.LearningToRank.Score (DocId, Rel)))
bestRankingPerFold bestPerFold' =
    fmap (\(testData, ~(model, _trainScore))  ->  rerankRankings' model testData) bestPerFold'



dumpKFoldModelsAndRankings
    :: forall f. (Ord f, Show f)
    => FoldRestartResults f
    -> ScoringMetric IsRelevant CAR.RunFile.QueryId QRel.DocumentName
    -> FilePath
    -> FilePath
    -> [IO ()]
dumpKFoldModelsAndRankings foldRestartResults metric outputFilePrefix modelFile =
    let bestPerFold' :: Folds (M.Map Q [(DocId, FeatureVec f Double, Rel)], (Model f, Double))
        bestPerFold' = bestPerFold foldRestartResults

        bestRankingPerFold' :: Folds (M.Map Q (Ranking SimplIR.LearningToRank.Score (DocId, Rel)))
        bestRankingPerFold' = bestRankingPerFold bestPerFold'

        testRanking ::   M.Map Q (Ranking SimplIR.LearningToRank.Score (DocId, Rel))
        testRanking = fold bestRankingPerFold'

        testScore = metric testRanking

--         dumpAll = [ do storeRankingData outputFilePrefix ranking metric modelDesc
--                        storeModelData outputFilePrefix modelFile model trainScore modelDesc
--                   | (foldNo, ~(testData, restartModels))  <- zip [0..] $ toList foldRestartResults
--                   , (restartNo, ~(model, trainScore)) <- zip [0..] restartModels
--                   , let ranking = rerankRankings' model testData
--                   , let modelDesc = "fold-"<> show foldNo <> "-restart-"<> show restartNo
--                   ]

        dumpAll = []

        dumpBest =
            [ do storeRankingData outputFilePrefix ranking metric modelDesc
                 storeModelData outputFilePrefix modelFile model trainScore modelDesc
            | (foldNo, (testData,  ~(model, trainScore)))  <- zip [0..] $ toList bestPerFold'
            , let ranking = rerankRankings' model testData
            , let modelDesc = "fold-"<> show foldNo <> "-best"
            ]

        dumpKfoldTestRanking = storeRankingData outputFilePrefix testRanking metric modelDesc
          where modelDesc = "test"

    in dumpAll ++ dumpBest ++ [dumpKfoldTestRanking]



dumpFullModelsAndRankings
    :: forall f. (Ord f, Show f)
    => M.Map Q [(DocId, FeatureVec f Double, Rel)]
    -> (Model f, Double)
    -> ScoringMetric IsRelevant CAR.RunFile.QueryId QRel.DocumentName
    -> FilePath
    -> FilePath
    -> [IO()]
dumpFullModelsAndRankings trainData (model, trainScore) metric outputFilePrefix modelFile =
    let modelDesc = "train"
        trainRanking = rerankRankings' model trainData
    in [ storeRankingData outputFilePrefix trainRanking metric modelDesc
       , storeModelData outputFilePrefix modelFile model trainScore modelDesc
       ]




l2rRankingToRankEntries :: CAR.RunFile.MethodName
                        -> M.Map CAR.RunFile.QueryId (Ranking SimplIR.LearningToRank.Score (QRel.DocumentName, Rel))
                        -> [CAR.RunFile.EntityRankingEntry]
l2rRankingToRankEntries methodName rankings =
  [ CAR.RunFile.RankingEntry { carQueryId = query
                             , carDocument = packPageId $ T.unpack doc
                             , carRank = rank
                             , carScore = rankScore
                             , carMethodName = methodName
                             }
  | (query, ranking) <- M.toList rankings
  , ((rankScore, (doc, rel)), rank) <- Ranking.toSortedList ranking `zip` [1..]
  ]



-- Train model on all data
storeModelData :: (Show f, Ord f)
               => FilePath
               -> FilePath
               -> Model f
               -> Double
               -> [Char]
               -> IO ()
storeModelData outputFilePrefix modelFile model trainScore modelDesc = do
  putStrLn $ "Model "++modelDesc++ " train metric "++ (show trainScore) ++ " MAP."
  let modelFile' = outputFilePrefix++modelFile++"-model-"++modelDesc++".json"
  BSL.writeFile modelFile' $ Data.Aeson.encode model
  putStrLn $ "Written model "++modelDesc++ " to file "++ (show modelFile') ++ " ."

storeRankingData ::  FilePath
               -> M.Map  CAR.RunFile.QueryId (Ranking Double (QRel.DocumentName, IsRelevant))
               -> ScoringMetric IsRelevant CAR.RunFile.QueryId QRel.DocumentName
               -> String
               -> IO ()
storeRankingData outputFilePrefix ranking metric modelDesc = do
  putStrLn $ "Model "++modelDesc++" test metric "++ show (metric ranking) ++ " MAP."
  CAR.RunFile.writeEntityRun (outputFilePrefix++"-model-"++modelDesc++".run")
       $ l2rRankingToRankEntries (CAR.RunFile.MethodName $ T.pack $ "l2r "++modelDesc)
       $ ranking

-- todo avoid duplicateion with storeRankingData
storeRankingDataNoMetric ::  FilePath
               -> M.Map CAR.RunFile.QueryId (Ranking Double (QRel.DocumentName, Rel))
               -> String
               -> IO ()
storeRankingDataNoMetric outputFilePrefix ranking modelDesc = do
  putStrLn $ "Model "++modelDesc++" .. no metric.."
  CAR.RunFile.writeEntityRun (outputFilePrefix++"-model-"++modelDesc++".run")
       $ l2rRankingToRankEntries (CAR.RunFile.MethodName $ T.pack $ "l2r "++modelDesc)
       $ ranking

