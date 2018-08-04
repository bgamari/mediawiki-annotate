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
import Data.Maybe
import Data.Foldable as Foldable
import Data.Function
import Data.Bifunctor
import Data.Hashable
import Data.Semigroup hiding (option)


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

type TrainData =  M.Map CAR.RunFile.QueryId [(QRel.DocumentName, Features, IsRelevant)]
type ReturnWithModelDiagnostics a = (a, [(String, Model, Double)])


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
