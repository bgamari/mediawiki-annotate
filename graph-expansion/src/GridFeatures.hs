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

import qualified Data.Set as S
import Data.Hashable

import CAR.Types hiding (Entity)

import SimplIR.LearningToRank
import qualified SimplIR.FeatureSpace as F
import SimplIR.FeatureSpace (featureDimension, FeatureSpace, FeatureVec, featureNames, mkFeatureSpace, concatSpace, concatFeatureVec)

import qualified CAR.RunFile as CAR.RunFile
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
data ExpansionModel = NoneX | Rm | Rm1 | EcmX | EcmRm | EcmPsg | EcmPsg1
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
             , (it, em) <- [ (EcmIdx, NoneX), (EcmIdx, Rm), (EcmIdx, EcmPsg), (EcmIdx, Rm1), (EcmIdx, EcmPsg1)
                           , (ParagraphIdx, NoneX), (ParagraphIdx, Rm), (ParagraphIdx, EcmPsg), (ParagraphIdx, Rm1), (ParagraphIdx, EcmPsg1)
                           ]
             ]



data GridRun = GridRun QueryModel RetrievalModel ExpansionModel IndexType
         deriving (Show, Read, Ord, Eq, Generic, Serialise, Hashable)

data Run = GridRun' GridRun | Aggr
         deriving (Show, Read, Ord, Eq, Generic, Serialise)
allEntityRunsF :: [Run]
allEntityRunsF = (GridRun' <$> entityRunsF) <> [Aggr]
allEdgeRunsF :: [Run]
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

--
-- onlyAggrEdge :: EdgeFeature -> Bool
-- onlyAggrEdge (EdgeRetrievalFeature Aggr runf) = True
-- onlyAggrEdge _  = False
--
-- onlyScoreEdge :: EdgeFeature -> Bool
-- onlyScoreEdge (EdgeRetrievalFeature _ ScoreF) = True
-- onlyScoreEdge _  = False
--
-- onlyRREdge :: EdgeFeature -> Bool
-- onlyRREdge (EdgeRetrievalFeature _ RecipRankF) = True
-- onlyRREdge _  = False
--
--
-- onlyLessFeaturesEdge :: EdgeFeature -> Bool
-- onlyLessFeaturesEdge (EdgeRetrievalFeature (GridRun' (GridRun _ _ NoneX ParagraphIdx)) _) = True
-- onlyLessFeaturesEdge (EdgeRetrievalFeature (GridRun' (GridRun _ _ Rm ParagraphIdx)) _) = True
-- onlyLessFeaturesEdge (EdgeRetrievalFeature (GridRun' (GridRun _ _ Rm1 ParagraphIdx)) _) = True
-- onlyLessFeaturesEdge (EdgeRetrievalFeature (GridRun' (GridRun _ _ EcmX ParagraphIdx)) _) = True
-- onlyLessFeaturesEdge (EdgeRetrievalFeature (GridRun' (GridRun _ _ EcmPsg ParagraphIdx)) _) = True
-- onlyLessFeaturesEdge (EdgeRetrievalFeature (GridRun' (GridRun _ _ EcmPsg1 ParagraphIdx)) _) = True
-- onlyLessFeaturesEdge _  = False
--
-- onlyNoneFeaturesEdge :: EdgeFeature -> Bool
-- onlyNoneFeaturesEdge (EdgeRetrievalFeature (GridRun' (GridRun _ _ NoneX _)) _) = True
-- onlyNoneFeaturesEdge _  = False

onlyPageEdge :: EdgeFeature -> Bool
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
onlyLessFeatures (Left (EntRetrievalFeature (GridRun' (GridRun _ _ Rm EntityIdx)) _)) = True
onlyLessFeatures (Left (EntRetrievalFeature (GridRun' (GridRun _ _ Rm PageIdx)) _)) = True
onlyLessFeatures (Left (EntRetrievalFeature (GridRun' (GridRun _ _ Rm1 EntityIdx)) _)) = True
onlyLessFeatures (Left (EntRetrievalFeature (GridRun' (GridRun _ _ Rm1 PageIdx)) _)) = True
onlyLessFeatures (Left (EntRetrievalFeature (GridRun' (GridRun _ _ EcmPsg EntityIdx)) _)) = True
onlyLessFeatures (Left (EntRetrievalFeature (GridRun' (GridRun _ _ EcmPsg PageIdx)) _)) = True
onlyLessFeatures (Left (EntRetrievalFeature (GridRun' (GridRun _ _ EcmPsg1 EntityIdx)) _)) = True
onlyLessFeatures (Left (EntRetrievalFeature (GridRun' (GridRun _ _ EcmPsg1 PageIdx)) _)) = True
onlyLessFeatures (Right (EdgeRetrievalFeature (GridRun' (GridRun _ _ NoneX ParagraphIdx)) _)) = True
onlyLessFeatures (Right (EdgeRetrievalFeature (GridRun' (GridRun _ _ Rm ParagraphIdx)) _)) = True
onlyLessFeatures (Right (EdgeRetrievalFeature (GridRun' (GridRun _ _ Rm1 ParagraphIdx)) _)) = True
onlyLessFeatures (Right (EdgeRetrievalFeature (GridRun' (GridRun _ _ EcmX ParagraphIdx)) _)) = True
onlyLessFeatures (Right (EdgeRetrievalFeature (GridRun' (GridRun _ _ EcmPsg ParagraphIdx)) _)) = True
onlyLessFeatures (Right (EdgeRetrievalFeature (GridRun' (GridRun _ _ EcmPsg1 ParagraphIdx)) _)) = True
onlyLessFeatures _  = False


-- GridRun  QueryModel RetrievalModel ExpansionModel IndexType
onlySimpleRmFeatures :: CombinedFeature -> Bool
onlySimpleRmFeatures (Left (EntRetrievalFeature (GridRun' (GridRun _ retrievalModel expansionModel indexType)) _)) =
    onlySimpleRmFeaturesHelper retrievalModel expansionModel indexType
onlySimpleRmFeatures (Right (EdgeRetrievalFeature (GridRun' (GridRun _ retrievalModel expansionModel indexType)) _)) =
    onlySimpleRmFeaturesHelper retrievalModel expansionModel indexType


onlySimpleRmFeaturesHelper :: RetrievalModel -> ExpansionModel -> IndexType -> Bool
onlySimpleRmFeaturesHelper retrievalModel expansionModel indexType =
     (indexType `S.member` S.fromList [EntityIdx, PageIdx, ParagraphIdx])
     &&  (expansionModel `S.member`  S.fromList [NoneX, Rm, EcmX, EcmPsg])
     &&  (retrievalModel == Bm25)

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




makeDefaultEntFeatVector :: F.FeatureVec EntityFeature Double
makeDefaultEntFeatVector = makeEntFeatVector []

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

makeDefaultEdgeFeatVector :: F.FeatureVec EdgeFeature Double
makeDefaultEdgeFeatVector = makeEdgeFeatVector []

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
      ScoreF -> rankScore entry
      RecipRankF -> recipRank entry
--       LinearRankF -> linearRank 100  entry
--       BucketRankF -> bucketRank entry
      CountF -> count entry
  where
    rankScore :: RankingEntry d -> Double
    rankScore entry  = CAR.RunFile.carScore entry

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



entityScoreVecFromMultiRankings :: MultiRankingEntry PageId GridRun -> [(EntityFeature, Double)]
entityScoreVecFromMultiRankings entityRankEntry =
      rankEntFeatures Aggr (multiRankingEntryCollapsed entityRankEntry)
      ++ concat [ rankEntFeatures (GridRun' g) entry
         | (g, entry) <- multiRankingEntryAll entityRankEntry
         ]


