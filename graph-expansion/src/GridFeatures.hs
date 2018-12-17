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


module GridFeatures where

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


minibatchParser :: Parser MiniBatchParams
minibatchParser = MiniBatchParams
    <$> option auto (long "mini-batch-steps" <> metavar "STEPS" <> help "iterations per mini-batch")
    <*> option auto (long "mini-batch-size" <> metavar "SIZE" <> help "number of mini-batch training queries")
    <*> option auto (long "mini-batch-eval" <> metavar "EVAL" <> help "number of mini-batches before next training evaluation")


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

data QueryModel = All | Title | SubTree | LeafHeading | Interior | SectionPath
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


onlyAggrEdge :: EdgeFeature -> Bool
onlyAggrEdge (EdgeRetrievalFeature Aggr runf) = True
onlyAggrEdge _  = False

onlyScoreEdge :: EdgeFeature -> Bool
onlyScoreEdge (EdgeRetrievalFeature _ ScoreF) = True
onlyScoreEdge _  = False

onlyRREdge :: EdgeFeature -> Bool
onlyRREdge (EdgeRetrievalFeature _ RecipRankF) = True
onlyRREdge _  = False


onlyLessFeaturesEdge :: EdgeFeature -> Bool
onlyLessFeaturesEdge (EdgeRetrievalFeature (GridRun' (GridRun _ _ NoneX ParagraphIdx)) _) = True
onlyLessFeaturesEdge (EdgeRetrievalFeature (GridRun' (GridRun _ _ Rm ParagraphIdx)) _) = True
onlyLessFeaturesEdge (EdgeRetrievalFeature (GridRun' (GridRun _ _ EcmX ParagraphIdx)) _) = True
onlyLessFeaturesEdge _  = False

onlyNoneFeaturesEdge :: EdgeFeature -> Bool
onlyNoneFeaturesEdge (EdgeRetrievalFeature (GridRun' (GridRun _ _ NoneX _)) _) = True
onlyNoneFeaturesEdge _  = False

onlyPageEdge (EdgeRetrievalFeature (GridRun' (GridRun Title _ _ _)) _) = True
onlyPageEdge (EdgeRetrievalFeature (GridRun' (GridRun All _ _ _)) _) = True
onlyPageEdge _ = False

filterExpSettings :: (Show f, Ord f)
                  => FeatureSpace f       -- ^ space to project into
                  -> FeatureVec f Double  -- ^ vector to project
                  -> FeatureVec f Double
filterExpSettings toFeatSpace features =
    F.fromList toFeatSpace
    $ [ pair
      | pair@(fname, _) <- F.toList features
      , fname `S.member` F.featureNameSet toFeatSpace
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


onlyLessFeatures :: CombinedFeature -> Bool
onlyLessFeatures (Left (EntRetrievalFeature (GridRun' (GridRun _ _ NoneX EntityIdx)) _)) = True
onlyLessFeatures (Left (EntRetrievalFeature (GridRun' (GridRun _ _ NoneX PageIdx)) _)) = True
onlyLessFeatures (Right (EdgeRetrievalFeature (GridRun' (GridRun _ _ NoneX ParagraphIdx)) _)) = True
onlyLessFeatures (Right (EdgeRetrievalFeature (GridRun' (GridRun _ _ Rm ParagraphIdx)) _)) = True
onlyLessFeatures (Right (EdgeRetrievalFeature (GridRun' (GridRun _ _ EcmX ParagraphIdx)) _)) = True
onlyLessFeatures _  = False

onlyNoneFeatures :: CombinedFeature -> Bool
onlyNoneFeatures (Left (EntRetrievalFeature (GridRun' (GridRun _ _ NoneX _)) _)) = True
onlyNoneFeatures (Right (EdgeRetrievalFeature (GridRun' (GridRun _ _ NoneX _)) _)) = True
onlyNoneFeatures _  = False


onlyPage :: CombinedFeature -> Bool
onlyPage (Left (EntRetrievalFeature (GridRun' (GridRun Title _ _ _)) _)) = True
onlyPage (Left (EntRetrievalFeature (GridRun' (GridRun All _ _ _)) _)) = True
onlyPage (Right (EdgeRetrievalFeature (GridRun' (GridRun Title _ _ _)) _)) = True
onlyPage (Right (EdgeRetrievalFeature (GridRun' (GridRun All _ _ _)) _)) = True
onlyPage _  = False

onlySection :: CombinedFeature -> Bool
onlySection (Left (EntRetrievalFeature (GridRun' (GridRun SubTree _ _ _)) _)) = True
onlySection (Left (EntRetrievalFeature (GridRun' (GridRun LeafHeading _ _ _)) _)) = True
onlySection (Left (EntRetrievalFeature (GridRun' (GridRun Interior _ _ _)) _)) = True
onlySection (Left (EntRetrievalFeature (GridRun' (GridRun GridFeatures.SectionPath _ _ _)) _)) = True
onlySection (Right (EdgeRetrievalFeature (GridRun' (GridRun SubTree _ _ _)) _)) = True
onlySection (Right (EdgeRetrievalFeature (GridRun' (GridRun LeafHeading _ _ _)) _)) = True
onlySection (Right (EdgeRetrievalFeature (GridRun' (GridRun Interior _ _ _)) _)) = True
onlySection (Right (EdgeRetrievalFeature (GridRun' (GridRun GridFeatures.SectionPath _ _ _)) _)) = True
onlySection _  = False


-- Right (EdgeRetrievalFeature (GridRun' QueryModel RetrievalModel ExpansionModel IndexType) RecipRankF)



-- -------------------------------------------
-- make feature vectors with defaults and stuff
-- -------------------------------------------

type EdgeFeatureVec = FeatureVec EdgeFeature Double
type EntityFeatureVec = FeatureVec EntityFeature Double
type CombinedFeatureVec = FeatureVec CombinedFeature Double




makeEntFeatVector :: [(EntityFeature, Double)] -> F.FeatureVec EntityFeature Double
makeEntFeatVector xs =
    F.modify defaults xs
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
    F.modify defaults xs
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
                  take nRestarts . trainWithRestarts miniBatchParams gen0 metric info fspace
                where
                  info = show foldIdx

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
                             , carScore = score
                             , carMethodName = methodName
                             }
  | (query, ranking) <- M.toList rankings
  , ((score, (doc, rel)), rank) <- Ranking.toSortedList ranking `zip` [1..]
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
--                -> TrainData
               -> _
               -> ScoringMetric IsRelevant CAR.RunFile.QueryId QRel.DocumentName
               -> String
               -> IO ()
storeRankingData outputFilePrefix ranking metric modelDesc = do

--   let rerankedFranking = rerankRankings' model evalData
  putStrLn $ "Model "++modelDesc++" test metric "++ show (metric ranking) ++ " MAP."
  CAR.RunFile.writeEntityRun (outputFilePrefix++"-model-"++modelDesc++".run")
       $ l2rRankingToRankEntries (CAR.RunFile.MethodName $ T.pack $ "l2r "++modelDesc)
       $ ranking

-- todo avoid duplicateion with storeRankingData
storeRankingDataNoMetric ::  FilePath
--                -> TrainData
               -> _
               -> String
               -> IO ()
storeRankingDataNoMetric outputFilePrefix ranking modelDesc = do

--   let rerankedFranking = rerankRankings' model evalData
  putStrLn $ "Model "++modelDesc++" .. no metric.."
  CAR.RunFile.writeEntityRun (outputFilePrefix++"-model-"++modelDesc++".run")
       $ l2rRankingToRankEntries (CAR.RunFile.MethodName $ T.pack $ "l2r "++modelDesc)
       $ ranking


newtype Folds a = Folds { getFolds :: [a] }
                deriving (Foldable, Functor, Traversable)
                deriving newtype (NFData)

newtype FoldIdx = FoldIdx Int
                deriving (Eq, Ord, Show, Enum)
numberFolds :: Folds a -> Folds (FoldIdx, a)
numberFolds (Folds xs) = Folds $ zip [FoldIdx 0..] xs

mkSequentialFolds :: Int -> [a] -> Folds [a]
mkSequentialFolds k xs = Folds $ chunksOf foldLen xs
  where
    foldLen
      | len >= 2 * k = (len `div` k) + 1  -- usual case: prevents overpopulation of last fold, e.g. [1,2] [3,4] [5,6] [7]
      | otherwise = len `div` k  -- to prevent last folds to be empty, accept overpopulation of last fold, e.g. [1] [2] [3] [4] [5,6,7]
    len = length xs

-- r might be: [(Model, Double)]


kFolds :: forall q docId rel r f.
       Eq q
       => (FoldIdx -> M.Map q [(docId, FeatureVec f Double, rel)] -> r)
       -> M.Map q [(docId, FeatureVec f Double, rel)]
          -- ^ training data
       -> Folds [q]
       -> Folds (M.Map q [(docId, FeatureVec f Double, rel)], r)
          -- ^ the training result and set of test data for the fold
kFolds train allData foldQueries =
    fmap trainSingleFold (numberFolds foldQueries)
  where
    trainSingleFold :: (FoldIdx, [q]) -> (M.Map q [(docId, FeatureVec f Double, rel)], r)
    trainSingleFold (foldIdx, testQueries) =
      let testData :: M.Map q [(docId, FeatureVec f Double, rel)]
          testData =  M.filterWithKey (\query _ -> query `elem` testQueries) allData

          trainData :: M.Map q [(docId, FeatureVec f Double, rel)]
          trainData =  M.filterWithKey (\query _ -> query `notElem` testQueries) allData
      in (testData, train foldIdx trainData)




-- ---------------------------------------------------------
-- ---------------------------------------------------------

entityScoreVecFromMultiRankings :: MultiRankingEntry PageId GridRun -> [(EntityFeature, Double)]
entityScoreVecFromMultiRankings entityRankEntry =
      rankEntFeatures Aggr (multiRankingEntryCollapsed entityRankEntry)
      ++ concat [ rankEntFeatures (GridRun' g) entry
         | (g, entry) <- multiRankingEntryAll entityRankEntry
         ]

