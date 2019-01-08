{-# LANGUAGE DataKinds #-}
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

import qualified Data.Map as M
import Data.Maybe
import Control.DeepSeq
import Options.Applicative
import Data.Aeson
import GHC.Generics
import Codec.Serialise

import qualified Data.Set as S
import Data.Hashable

import CAR.Types hiding (Entity)

import SimplIR.LearningToRank
import qualified SimplIR.FeatureSpace as F
import SimplIR.FeatureSpace (FeatureSpace, FeatureVec, featureNames, mkFeatureSpace)

import qualified CAR.RunFile as CAR.RunFile
import MultiTrecRunFile

import qualified Debug.Trace as Debug

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
                           ++ [(EntityIdx, em) | em <- [NoneX, Rm]] -- for edges derived from pages
                           ++ [(PageIdx, em) | em <- [NoneX, Rm]]  -- for edges derived from pages
             ]



data GridRun = GridRun !QueryModel !RetrievalModel !ExpansionModel !IndexType
         deriving (Show, Read, Ord, Eq, Generic, Serialise, Hashable)

instance NFData GridRun where
    rnf x = x `seq` ()

data Run = GridRun' GridRun | Aggr
         deriving (Show, Read, Ord, Eq, Generic, Serialise)
allEntityRunsF :: [Run]
allEntityRunsF = (GridRun' <$> entityRunsF) <> [Aggr]
allEdgeRunsF :: [Run]
allEdgeRunsF = (GridRun' <$> edgeRunsF) <> [Aggr]
allSources :: [FromSource]
allSources = [FromParas, FromPagesOwnerLink, FromPagesLinkOwner, FromPagesLinkLink, FromPagesSelf]

data RunFeature = ScoreF | RecipRankF | CountF --LinearRankF | BucketRankF
         deriving (Show, Read, Ord, Eq, Enum, Bounded, Generic, Serialise)
data FromSource = FromParas | FromPagesOwnerLink | FromPagesLinkOwner | FromPagesLinkLink  | FromPagesSelf
         deriving (Show, Read, Ord, Eq, Enum, Bounded, Generic, Serialise)

allRunFeatures :: [RunFeature]
allRunFeatures = [ScoreF, RecipRankF] --[minBound..maxBound]

data EntityFeature where
    EntRetrievalFeature :: !Run -> !RunFeature -> EntityFeature
    EntIncidentEdgeDocsRecip :: EntityFeature
    EntDegreeRecip :: EntityFeature
    EntDegree  :: EntityFeature
    deriving (Show, Read, Ord, Eq)

data EdgeFeature where
    EdgeRetrievalFeature :: !FromSource -> !Run -> !RunFeature -> EdgeFeature
    EdgeDocKL  :: !FromSource -> EdgeFeature
    EdgeCount  :: !FromSource -> EdgeFeature
    deriving (Show, Read, Ord, Eq)

data EntityOrEdge = Entity | Edge
         deriving (Show, Read, Ord, Eq, Enum, Bounded, Generic, Serialise)

type CombinedFeature = Either EntityFeature EdgeFeature

allEntityFeatures :: S.Set EntityFeature
allEntityFeatures = S.fromList $
    (EntRetrievalFeature <$> allEntityRunsF <*> allRunFeatures)
    <> [EntIncidentEdgeDocsRecip, EntDegreeRecip, EntDegree]


allEdgeFeatures :: S.Set EdgeFeature
allEdgeFeatures = S.fromList $
    (EdgeRetrievalFeature <$> allSources <*> allEdgeRunsF <*> allRunFeatures)
    <> ([EdgeDocKL, EdgeCount] <*>  allSources)

-- todo can we get rid of this?
entSomeFSpace :: F.SomeFeatureSpace EntityFeature
entSomeFSpace = F.mkFeatureSpace allEntityFeatures

edgeSomeFSpace :: F.SomeFeatureSpace EdgeFeature
edgeSomeFSpace = F.mkFeatureSpace allEdgeFeatures




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
onlyPageEdge (EdgeRetrievalFeature  _ (GridRun' (GridRun Title _ _ _)) _) = True
onlyPageEdge (EdgeRetrievalFeature  _ (GridRun' (GridRun All _ _ _)) _) = True
onlyPageEdge _ = False

filterExpSettings :: (Show f, Ord f)
                  => FeatureSpace f s        -- ^ space to project into
                  -> FeatureVec f s' Double  -- ^ vector to project
                  -> FeatureVec f s Double
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
onlyAggr (Right (EdgeRetrievalFeature _ Aggr runf)) = True
onlyAggr _  = False

onlyScore :: CombinedFeature -> Bool
onlyScore (Left (EntRetrievalFeature _ ScoreF)) = True
onlyScore (Right (EdgeRetrievalFeature _ _ ScoreF)) = True
onlyScore x = nothingElseButAggr x

onlyRR :: CombinedFeature -> Bool
onlyRR (Left (EntRetrievalFeature _ RecipRankF)) = True
onlyRR (Right (EdgeRetrievalFeature _ _ RecipRankF)) = True
onlyRR x = nothingElseButAggr x


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
onlyLessFeatures (Right (EdgeRetrievalFeature _ (GridRun' (GridRun _ _ NoneX ParagraphIdx)) _)) = True
onlyLessFeatures (Right (EdgeRetrievalFeature _ (GridRun' (GridRun _ _ Rm ParagraphIdx)) _)) = True
onlyLessFeatures (Right (EdgeRetrievalFeature _ (GridRun' (GridRun _ _ Rm1 ParagraphIdx)) _)) = True
onlyLessFeatures (Right (EdgeRetrievalFeature _ (GridRun' (GridRun _ _ EcmX ParagraphIdx)) _)) = True
onlyLessFeatures (Right (EdgeRetrievalFeature _ (GridRun' (GridRun _ _ EcmPsg ParagraphIdx)) _)) = True
onlyLessFeatures (Right (EdgeRetrievalFeature _ (GridRun' (GridRun _ _ EcmPsg1 ParagraphIdx)) _)) = True
onlyLessFeatures x = nothingElseButAggr x


-- GridRun  QueryModel RetrievalModel ExpansionModel IndexType
onlySimpleRmFeatures :: CombinedFeature -> Bool
onlySimpleRmFeatures (Left (EntRetrievalFeature (GridRun' (GridRun _ retrievalModel expansionModel indexType)) _)) =
    onlySimpleRmFeaturesHelper retrievalModel expansionModel indexType
onlySimpleRmFeatures (Right (EdgeRetrievalFeature _ (GridRun' (GridRun _ retrievalModel expansionModel indexType)) _)) =
    onlySimpleRmFeaturesHelper retrievalModel expansionModel indexType
onlySimpleRmFeatures (Left (EntRetrievalFeature Aggr runf)) = True
onlySimpleRmFeatures (Right (EdgeRetrievalFeature _ Aggr runf)) = True
onlySimpleRmFeatures other = Debug.trace ("Warning: onlySimpleRmFeatures did not specify this feature: " <> show other <> " Returning True.")    True


onlySimpleRmFeaturesHelper :: RetrievalModel -> ExpansionModel -> IndexType -> Bool
onlySimpleRmFeaturesHelper retrievalModel expansionModel indexType =
     (indexType `S.member` S.fromList [EntityIdx, PageIdx, ParagraphIdx])
     &&  (expansionModel `S.member`  S.fromList [NoneX, Rm, EcmX, EcmPsg])
     &&  (retrievalModel == Bm25)

onlyNoneFeatures :: CombinedFeature -> Bool
onlyNoneFeatures (Left (EntRetrievalFeature (GridRun' (GridRun _ _ NoneX _)) _)) = True
onlyNoneFeatures (Right (EdgeRetrievalFeature _ (GridRun' (GridRun _ _ NoneX _)) _)) = True
onlyNoneFeatures x  = nothingElseButAggr x


onlyPage :: CombinedFeature -> Bool
onlyPage (Left (EntRetrievalFeature (GridRun' (GridRun Title _ _ _)) _)) = True
onlyPage (Left (EntRetrievalFeature (GridRun' (GridRun All _ _ _)) _)) = True
onlyPage (Right (EdgeRetrievalFeature _ (GridRun' (GridRun Title _ _ _)) _)) = True
onlyPage (Right (EdgeRetrievalFeature _ (GridRun' (GridRun All _ _ _)) _)) = True
onlyPage x = nothingElseButAggr x

onlyTitleAndSectionPath :: CombinedFeature -> Bool
onlyTitleAndSectionPath  (Left (EntRetrievalFeature (GridRun' (GridRun Title _ _ _)) _)) = True
onlyTitleAndSectionPath  (Left (EntRetrievalFeature (GridRun' (GridRun GridFeatures.SectionPath _ _ _)) _)) = True
onlyTitleAndSectionPath  (Right (EdgeRetrievalFeature _ (GridRun' (GridRun Title _ _ _)) _)) = True
onlyTitleAndSectionPath  (Right (EdgeRetrievalFeature _ (GridRun' (GridRun GridFeatures.SectionPath _ _ _)) _)) = True
onlyTitleAndSectionPath (Left (EntRetrievalFeature Aggr runf)) = True
onlyTitleAndSectionPath (Right (EdgeRetrievalFeature _ Aggr runf)) = True
onlyTitleAndSectionPath x = nothingElseButAggr x

onlySection :: CombinedFeature -> Bool
onlySection (Left (EntRetrievalFeature (GridRun' (GridRun SubTree _ _ _)) _)) = True
onlySection (Left (EntRetrievalFeature (GridRun' (GridRun LeafHeading _ _ _)) _)) = True
onlySection (Left (EntRetrievalFeature (GridRun' (GridRun Interior _ _ _)) _)) = True
onlySection (Left (EntRetrievalFeature (GridRun' (GridRun GridFeatures.SectionPath _ _ _)) _)) = True
onlySection (Right (EdgeRetrievalFeature _ (GridRun' (GridRun SubTree _ _ _)) _)) = True
onlySection (Right (EdgeRetrievalFeature _ (GridRun' (GridRun LeafHeading _ _ _)) _)) = True
onlySection (Right (EdgeRetrievalFeature _ (GridRun' (GridRun Interior _ _ _)) _)) = True
onlySection (Right (EdgeRetrievalFeature _ (GridRun' (GridRun GridFeatures.SectionPath _ _ _)) _)) = True
onlySection x = nothingElseButAggr x



noEdgesFromParas :: CombinedFeature -> Bool
noEdgesFromParas (Left _) = True
noEdgesFromParas (Right (EdgeRetrievalFeature FromParas (_) _)) = False
noEdgesFromParas (Right (EdgeRetrievalFeature FromPagesOwnerLink (_) _)) = True
noEdgesFromParas (Right (EdgeRetrievalFeature FromPagesLinkOwner (_) _)) = True
noEdgesFromParas (Right (EdgeRetrievalFeature FromPagesLinkLink (_) _)) = True
noEdgesFromParas (Right (EdgeRetrievalFeature FromPagesSelf (_) _)) = True
noEdgesFromParas x = nothingElseButAggr x

-- FromParas, FromPagesOwnerLink, FromPagesLinkOwner, FromPagesLinkLink, FromPagesSelf]

noEdgesFromPages :: CombinedFeature -> Bool
noEdgesFromPages (Left _) = True
noEdgesFromPages (Right (EdgeRetrievalFeature FromParas (_) _)) = True
noEdgesFromPages (Right (EdgeRetrievalFeature FromPagesOwnerLink (_) _)) = False
noEdgesFromPages (Right (EdgeRetrievalFeature FromPagesLinkOwner (_) _)) = False
noEdgesFromPages (Right (EdgeRetrievalFeature FromPagesLinkLink (_) _)) = False
noEdgesFromPages (Right (EdgeRetrievalFeature FromPagesSelf (_) _)) = False
noEdgesFromPages x = nothingElseButAggr x

noEdgesFromPageLinkLink :: CombinedFeature -> Bool
noEdgesFromPageLinkLink (Left _) = True
noEdgesFromPageLinkLink (Right (EdgeRetrievalFeature FromParas (_) _)) = True
noEdgesFromPageLinkLink (Right (EdgeRetrievalFeature FromPagesOwnerLink (_) _)) = True
noEdgesFromPageLinkLink (Right (EdgeRetrievalFeature FromPagesLinkOwner (_) _)) = True
noEdgesFromPageLinkLink (Right (EdgeRetrievalFeature FromPagesLinkLink (_) _)) = False
noEdgesFromPageLinkLink (Right (EdgeRetrievalFeature FromPagesSelf (_) _)) = True
noEdgesFromPageLinkLink x = nothingElseButAggr x



nothingElseButAggr :: CombinedFeature -> Bool
nothingElseButAggr (Left (EntRetrievalFeature Aggr _)) = True
nothingElseButAggr (Right (EdgeRetrievalFeature _ Aggr _)) = True
nothingElseButAggr _ = False


-- Right (EdgeRetrievalFeature (GridRun' QueryModel RetrievalModel ExpansionModel IndexType) RecipRankF)



-- -------------------------------------------
-- make feature vectors with defaults and stuff
-- -------------------------------------------

type EdgeFeatureVec s     = FeatureVec EdgeFeature s Double
type EntityFeatureVec s   = FeatureVec EntityFeature s Double
type CombinedFeatureVec entFspace edgeFspace = FeatureVec CombinedFeature (F.Stack '[entFspace, edgeFspace]) Double

makeDefaultEntFeatVector :: F.FeatureSpace EntityFeature s -> F.FeatureVec EntityFeature s Double
makeDefaultEntFeatVector fspace = makeEntFeatVector fspace []

makeEntFeatVector :: FeatureSpace EntityFeature s
                  -> [(EntityFeature, Double)]
                  -> F.FeatureVec EntityFeature s Double
makeEntFeatVector fspace xs =
    let xs' = M.fromList xs
    in F.generate fspace $ (\f -> fromMaybe (defaultEntityFeatures f) $ M.lookup f xs')

defaultEntityFeatures :: EntityFeature -> Double
defaultEntityFeatures f =
    case f of
      EntIncidentEdgeDocsRecip -> 0.0
      EntDegreeRecip -> 0.0
      EntDegree -> 0.0
      EntRetrievalFeature _ runF -> defaultRankFeatures runF

makeDefaultEdgeFeatVector :: F.FeatureSpace EdgeFeature s -> F.FeatureVec EdgeFeature s Double
makeDefaultEdgeFeatVector fspace = makeEdgeFeatVector fspace []

makeEdgeFeatVector :: FeatureSpace EdgeFeature s
                   -> [(EdgeFeature, Double)]
                   -> F.FeatureVec EdgeFeature s Double
makeEdgeFeatVector fspace xs =
    let xs' = M.fromList xs
    in F.generate fspace $ \f -> fromMaybe (defaultEdgeFeatures f) $ M.lookup f xs'

defaultEdgeFeatures :: EdgeFeature -> Double
defaultEdgeFeatures f =
    case f of
      EdgeCount _ -> 0
      EdgeDocKL _ -> 0
      EdgeRetrievalFeature _ _ runF -> defaultRankFeatures runF

defaultRankFeatures :: RunFeature -> Double
defaultRankFeatures runF =
    case runF of
      ScoreF -> 0.0
      RecipRankF -> 0.0
--       LinearRankF -> 0.0
--       BucketRankF -> 0.0
      CountF -> 0.0

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

rankEdgeFeatures :: FromSource -> Run -> RankingEntry d -> [(EdgeFeature, Double)]
rankEdgeFeatures source run entry =
    [ (EdgeRetrievalFeature source run runF, rankFeatures runF entry)
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


