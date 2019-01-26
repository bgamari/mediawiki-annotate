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
import AspectUtils

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
data RetrievalModel = Bm25 | Ql | Sdm
         deriving (Show, Read, Ord, Eq, Enum, Bounded, Generic, Serialise, Hashable)
data ExpansionModel = NoneX | Rm | Rm1 | EcmX | EcmRm | EcmPsg | EcmPsg1
         deriving (Show, Read, Ord, Eq, Enum, Bounded, Generic, Serialise, Hashable)
data IndexType = EcmIdx | EntityIdx | PageIdx | ParagraphIdx | AspectIdx
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
                           ++ [(AspectIdx, em) | em <- [minBound..maxBound]]
             ]

pageEdgeRunsF :: [GridRun]
pageEdgeRunsF =    [ GridRun qm rm em it
                   | qm <- [minBound..maxBound]
                   , rm <- [minBound..maxBound]
                   , (it, em) <- [(EntityIdx, em) | em <- [NoneX, Rm, EcmPsg]] -- for edges derived from pages
                                 ++ [(PageIdx, em) | em <- [NoneX, Rm, EcmPsg]]  -- for edges derived from pages
                   ]
paraEdgeRunsF :: [GridRun]
paraEdgeRunsF = [ GridRun qm rm em it
                | qm <- [minBound..maxBound]
                , rm <- [minBound..maxBound]
                , (it, em) <- [ (EcmIdx, NoneX), (EcmIdx, Rm), (EcmIdx, EcmPsg), (EcmIdx, Rm1), (EcmIdx, EcmPsg1)
                              , (ParagraphIdx, NoneX), (ParagraphIdx, Rm), (ParagraphIdx, EcmPsg), (ParagraphIdx, Rm1), (ParagraphIdx, EcmPsg1)]
                ]
aspectEdgeRunsF :: [GridRun]
aspectEdgeRunsF =  [ GridRun qm rm em it
                   | qm <- [minBound..maxBound]
                   , rm <- [minBound..maxBound]
                   , (it, em) <-  [(AspectIdx, em) | em <- [NoneX, Rm, EcmPsg]]  -- for edges derived from aspects
                   ]

-- edgeRunsF :: [GridRun]
-- edgeRunsF = [ GridRun qm rm em it
--              | qm <- [minBound..maxBound]
--              , rm <- [minBound..maxBound]
--              , (it, em) <- [ (EcmIdx, NoneX), (EcmIdx, Rm), (EcmIdx, EcmPsg), (EcmIdx, Rm1), (EcmIdx, EcmPsg1)
--                            , (ParagraphIdx, NoneX), (ParagraphIdx, Rm), (ParagraphIdx, EcmPsg), (ParagraphIdx, Rm1), (ParagraphIdx, EcmPsg1)
--                            ]
--                            ++ [(EntityIdx, em) | em <- [NoneX, Rm, EcmPsg]] -- for edges derived from pages
--                            ++ [(PageIdx, em) | em <- [NoneX, Rm, EcmPsg]]  -- for edges derived from pages
--                            ++ [(AspectIdx, em) | em <- [NoneX, Rm, EcmPsg]]  -- for edges derived from pages
--              ]


data GridRun = GridRun !QueryModel !RetrievalModel !ExpansionModel !IndexType
         deriving (Show, Read, Ord, Eq, Generic, Serialise, Hashable)

instance NFData GridRun where
    rnf x = x `seq` ()

data Run = GridRun' GridRun | Aggr
         deriving (Show, Read, Ord, Eq, Generic, Serialise)

allEntityRunsF :: [Run]
allEntityRunsF = (GridRun' <$> entityRunsF) <> [Aggr]

pageSources, paraSources, aspectSources, allSources :: [FromSource]
pageSources = [FromPagesOwnerLink, FromPagesLinkOwner, FromPagesLinkLink, FromPagesSelf]
paraSources = [FromParas]
aspectSources = [FromAspectsOwnerLink, FromAspectsLinkOwner, FromAspectsLinkLink, FromAspectsSelf]
allSources = pageSources <> paraSources <> aspectSources

data RunFeature = ScoreF | RecipRankF | CountF --LinearRankF | BucketRankF
         deriving (Show, Read, Ord, Eq, Enum, Bounded, Generic, Serialise)
data FromSource = FromParas
                | FromPagesOwnerLink | FromPagesLinkOwner | FromPagesLinkLink  | FromPagesSelf
                | FromAspectsOwnerLink | FromAspectsLinkOwner | FromAspectsLinkLink  | FromAspectsSelf
         deriving (Show, Read, Ord, Eq, Enum, Bounded, Generic, Serialise)

allRunFeatures :: [RunFeature]
allRunFeatures = [ScoreF, RecipRankF, CountF] --[minBound..maxBound]

data EntityFeature where
    EntRetrievalFeature :: !Run -> !RunFeature -> EntityFeature
--     EntIncidentEdgeDocsRecip :: EntityFeature
--     EntDegreeRecip :: EntityFeature
    EntDegree  :: EntityFeature
    deriving (Show, Read, Ord, Eq, Generic, Serialise)

data EdgeFeature where
    EdgeRetrievalFeature :: !FromSource -> !Run -> !RunFeature -> EdgeFeature
    NeighborFeature :: !EntityFeature -> EdgeFeature
    NeighborSourceFeature :: !FromSource -> !EntityFeature -> EdgeFeature
    NeighborSourceScaleFeature :: !FromSource -> !EntityFeature -> EdgeFeature
--     EdgeDocKL  :: !FromSource -> EdgeFeature
    EdgeCount  :: !FromSource -> EdgeFeature
    deriving (Show, Read, Ord, Eq, Generic, Serialise)

data EntityOrEdge = Entity | Edge | Aspect
         deriving (Show, Read, Ord, Eq, Enum, Bounded, Generic, Serialise)

type CombinedFeature = Either EntityFeature EdgeFeature

allEntityFeatures :: S.Set EntityFeature
allEntityFeatures = S.fromList $
    (EntRetrievalFeature <$> allEntityRunsF <*> allRunFeatures)
    <> [EntDegree]
--     <> [EntIncidentEdgeDocsRecip, EntDegreeRecip, EntDegree]


allEdgeFeatures :: S.Set EdgeFeature
allEdgeFeatures = S.fromList $
    (EdgeRetrievalFeature <$> paraSources <*> withAggr paraEdgeRunsF <*> allRunFeatures)
    <> (EdgeRetrievalFeature <$> pageSources <*> withAggr pageEdgeRunsF <*> allRunFeatures)
    <> (EdgeRetrievalFeature <$> aspectSources <*> withAggr aspectEdgeRunsF <*> allRunFeatures)
    <> ([EdgeCount] <*>  allSources)
    <> (NeighborFeature <$> (S.toList allEntityFeatures))
    <> (NeighborSourceFeature <$> allSources <*> (S.toList allEntityFeatures))
    <> (NeighborSourceScaleFeature <$> allSources <*> (S.toList allEntityFeatures))
  where withAggr edgeRunsF = (GridRun' <$> edgeRunsF) <> [Aggr]

-- todo can we get rid of this?
entSomeFSpace :: F.SomeFeatureSpace EntityFeature
entSomeFSpace = F.mkFeatureSpace allEntityFeatures

edgeSomeFSpace :: F.SomeFeatureSpace EdgeFeature
edgeSomeFSpace = F.mkFeatureSpace allEdgeFeatures




-- -------------------------------------------
-- filtering of feature spaces
-- -------------------------------------------


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

noNeighborFeats :: CombinedFeature -> Bool
noNeighborFeats (Right (NeighborFeature _)) = False
noNeighborFeats (Right (NeighborSourceFeature _ _)) = False
noNeighborFeats (Right (NeighborSourceScaleFeature _ _)) = False
noNeighborFeats _  = True

noRawEdge :: CombinedFeature -> Bool
noRawEdge (Right (EdgeRetrievalFeature _ _ _)) = False
noRawEdge _  = True

onlyAggr :: CombinedFeature -> Bool
onlyAggr (Left (EntRetrievalFeature Aggr _runf)) = True
onlyAggr (Right (EdgeRetrievalFeature _ Aggr _runf)) = True
onlyAggr (Right (NeighborFeature (EntRetrievalFeature Aggr _runf))) = True
onlyAggr (Right (NeighborSourceFeature _ (EntRetrievalFeature Aggr _runf))) = True
onlyAggr (Right (NeighborSourceScaleFeature _ (EntRetrievalFeature Aggr _runf))) = True
onlyAggr _  = False

onlyScore :: CombinedFeature -> Bool
onlyScore (Left (EntRetrievalFeature _ ScoreF)) = True
onlyScore (Right (EdgeRetrievalFeature _ _ ScoreF)) = True
onlyScore (Right (NeighborFeature (EntRetrievalFeature _ ScoreF))) = True
onlyScore (Right (NeighborSourceFeature _ (EntRetrievalFeature _ ScoreF))) = True
onlyScore (Right (NeighborSourceScaleFeature _ (EntRetrievalFeature _ ScoreF))) = True
onlyScore x = nothingElseButAggr x

onlyRR :: CombinedFeature -> Bool
onlyRR (Left (EntRetrievalFeature _ RecipRankF)) = True
onlyRR (Right (EdgeRetrievalFeature _ _ RecipRankF)) = True
onlyRR (Right (NeighborFeature (EntRetrievalFeature _ RecipRankF))) = True
onlyRR (Right (NeighborSourceFeature _ (EntRetrievalFeature _ RecipRankF))) = True
onlyRR (Right (NeighborSourceScaleFeature _ (EntRetrievalFeature _ RecipRankF))) = True
onlyRR x = nothingElseButAggr x

onlyCount :: CombinedFeature -> Bool
onlyCount (Left (EntRetrievalFeature _ CountF)) = True
onlyCount (Right (EdgeRetrievalFeature _ _ CountF)) = True
onlyCount (Right (NeighborFeature (EntRetrievalFeature _ CountF))) = True
onlyCount (Right (NeighborSourceFeature _ (EntRetrievalFeature _ CountF))) = True
onlyCount (Right (NeighborSourceScaleFeature _ (EntRetrievalFeature _ CountF))) = True
onlyCount x = nothingElseButAggr x


onlyLessFeatures :: CombinedFeature -> Bool
onlyLessFeatures (Left (EntRetrievalFeature (GridRun' (GridRun _ _ NoneX EntityIdx)) _)) = True
onlyLessFeatures (Left (EntRetrievalFeature (GridRun' (GridRun _ _ NoneX PageIdx)) _)) = True
onlyLessFeatures (Left (EntRetrievalFeature (GridRun' (GridRun _ _ NoneX AspectIdx)) _)) = True
onlyLessFeatures (Left (EntRetrievalFeature (GridRun' (GridRun _ _ Rm EntityIdx)) _)) = True
onlyLessFeatures (Left (EntRetrievalFeature (GridRun' (GridRun _ _ Rm PageIdx)) _)) = True
onlyLessFeatures (Left (EntRetrievalFeature (GridRun' (GridRun _ _ Rm AspectIdx)) _)) = True
onlyLessFeatures (Left (EntRetrievalFeature (GridRun' (GridRun _ _ Rm1 EntityIdx)) _)) = True
onlyLessFeatures (Left (EntRetrievalFeature (GridRun' (GridRun _ _ Rm1 PageIdx)) _)) = True
onlyLessFeatures (Left (EntRetrievalFeature (GridRun' (GridRun _ _ Rm1 AspectIdx)) _)) = True
onlyLessFeatures (Left (EntRetrievalFeature (GridRun' (GridRun _ _ EcmPsg EntityIdx)) _)) = True
onlyLessFeatures (Left (EntRetrievalFeature (GridRun' (GridRun _ _ EcmPsg PageIdx)) _)) = True
onlyLessFeatures (Left (EntRetrievalFeature (GridRun' (GridRun _ _ EcmPsg AspectIdx)) _)) = True
onlyLessFeatures (Left (EntRetrievalFeature (GridRun' (GridRun _ _ EcmPsg1 EntityIdx)) _)) = True
onlyLessFeatures (Left (EntRetrievalFeature (GridRun' (GridRun _ _ EcmPsg1 PageIdx)) _)) = True
onlyLessFeatures (Left (EntRetrievalFeature (GridRun' (GridRun _ _ EcmPsg1 AspectIdx)) _)) = True
onlyLessFeatures (Right (EdgeRetrievalFeature _ (GridRun' (GridRun _ _ NoneX ParagraphIdx)) _)) = True
onlyLessFeatures (Right (EdgeRetrievalFeature _ (GridRun' (GridRun _ _ Rm ParagraphIdx)) _)) = True
onlyLessFeatures (Right (EdgeRetrievalFeature _ (GridRun' (GridRun _ _ Rm1 ParagraphIdx)) _)) = True
onlyLessFeatures (Right (EdgeRetrievalFeature _ (GridRun' (GridRun _ _ EcmX ParagraphIdx)) _)) = True
onlyLessFeatures (Right (EdgeRetrievalFeature _ (GridRun' (GridRun _ _ EcmPsg ParagraphIdx)) _)) = True
onlyLessFeatures (Right (EdgeRetrievalFeature _ (GridRun' (GridRun _ _ EcmPsg1 ParagraphIdx)) _)) = True
onlyLessFeatures (Right (NeighborFeature entF)) = onlyLessFeatures (Left entF)
onlyLessFeatures (Right (NeighborSourceFeature _ entF)) = onlyLessFeatures (Left entF)
onlyLessFeatures (Right (NeighborSourceScaleFeature _ entF)) = onlyLessFeatures (Left entF)
onlyLessFeatures x = nothingElseButAggr x


-- GridRun  QueryModel RetrievalModel ExpansionModel IndexType
onlySimpleRmFeatures :: CombinedFeature -> Bool
onlySimpleRmFeatures (Left (EntRetrievalFeature (GridRun' (GridRun _ retrievalModel expansionModel indexType)) _)) =
    onlySimpleRmFeaturesHelper retrievalModel expansionModel indexType
onlySimpleRmFeatures (Right (EdgeRetrievalFeature _ (GridRun' (GridRun _ retrievalModel expansionModel indexType)) _)) =
    onlySimpleRmFeaturesHelper retrievalModel expansionModel indexType
onlySimpleRmFeatures (Left (EntRetrievalFeature Aggr _runf)) = True
onlySimpleRmFeatures (Right (EdgeRetrievalFeature _ Aggr _runf)) = True
onlySimpleRmFeatures (Left (EntDegree)) = False
onlySimpleRmFeatures (Right (EdgeCount _)) = False
onlySimpleRmFeatures (Right (NeighborFeature entF)) = onlySimpleRmFeatures (Left entF)
onlySimpleRmFeatures (Right (NeighborSourceFeature  _ entF)) = onlySimpleRmFeatures (Left entF)
onlySimpleRmFeatures (Right (NeighborSourceScaleFeature  _ entF)) = onlySimpleRmFeatures (Left entF)
onlySimpleRmFeatures other = Debug.trace ("Warning: onlySimpleRmFeatures did not specify this feature: " <> show other <> " Returning True.")    True


onlySimpleRmFeaturesHelper :: RetrievalModel -> ExpansionModel -> IndexType -> Bool
onlySimpleRmFeaturesHelper retrievalModel expansionModel indexType =
     (indexType `S.member` S.fromList [EntityIdx, PageIdx, ParagraphIdx, AspectIdx])
     &&  (expansionModel `S.member`  S.fromList [NoneX, Rm, EcmX, EcmPsg])
     &&  (retrievalModel == Bm25 || retrievalModel == Sdm)


-- GridRun  QueryModel RetrievalModel ExpansionModel IndexType
onlySimpleRmCountFeatures :: CombinedFeature -> Bool
onlySimpleRmCountFeatures (Left (EntRetrievalFeature (GridRun' (GridRun _ retrievalModel expansionModel indexType)) _)) =
    onlySimpleRmCountFeaturesHelper retrievalModel expansionModel indexType
onlySimpleRmCountFeatures (Right (EdgeRetrievalFeature _ (GridRun' (GridRun _ retrievalModel expansionModel indexType)) _)) =
    onlySimpleRmCountFeaturesHelper retrievalModel expansionModel indexType
onlySimpleRmCountFeatures (Left (EntRetrievalFeature Aggr _runf)) = True
onlySimpleRmCountFeatures (Right (EdgeRetrievalFeature _ Aggr _runf)) = True
onlySimpleRmCountFeatures (Left (EntDegree)) = True
onlySimpleRmCountFeatures (Right (EdgeCount _)) = True
onlySimpleRmCountFeatures (Right (NeighborFeature entF)) = onlySimpleRmCountFeatures (Left entF)
onlySimpleRmCountFeatures (Right (NeighborSourceFeature  _ entF)) = onlySimpleRmCountFeatures (Left entF)
onlySimpleRmCountFeatures (Right (NeighborSourceScaleFeature  _ entF)) = onlySimpleRmCountFeatures (Left entF)
onlySimpleRmCountFeatures other = Debug.trace ("Warning: onlySimpleRmFeatures did not specify this feature: " <> show other <> " Returning True.")    True

onlySimpleRmCountFeaturesHelper :: RetrievalModel -> ExpansionModel -> IndexType -> Bool
onlySimpleRmCountFeaturesHelper retrievalModel expansionModel indexType =
     (indexType `S.member` S.fromList [PageIdx, ParagraphIdx, AspectIdx])
     &&  (expansionModel `S.member`  S.fromList [NoneX, Rm, EcmX])
     &&  (retrievalModel == Bm25 || retrievalModel == Sdm)


onlyNoneFeatures :: CombinedFeature -> Bool
onlyNoneFeatures (Left (EntRetrievalFeature (GridRun' (GridRun _ _ NoneX _)) _)) = True
onlyNoneFeatures (Right (EdgeRetrievalFeature _ (GridRun' (GridRun _ _ NoneX _)) _)) = True
onlyNoneFeatures (Right (NeighborFeature entF)) = onlyNoneFeatures (Left entF)
onlyNoneFeatures (Right (NeighborSourceFeature _ entF)) = onlyNoneFeatures (Left entF)
onlyNoneFeatures (Right (NeighborSourceScaleFeature _ entF)) = onlyNoneFeatures (Left entF)
onlyNoneFeatures x  = nothingElseButAggr x


onlyPage :: CombinedFeature -> Bool
onlyPage (Left (EntRetrievalFeature (GridRun' (GridRun Title _ _ _)) _)) = True
onlyPage (Left (EntRetrievalFeature (GridRun' (GridRun All _ _ _)) _)) = True
onlyPage (Right (EdgeRetrievalFeature _ (GridRun' (GridRun Title _ _ _)) _)) = True
onlyPage (Right (EdgeRetrievalFeature _ (GridRun' (GridRun All _ _ _)) _)) = True
onlyPage (Right (NeighborFeature entF)) = onlyPage (Left entF)
onlyPage (Right (NeighborSourceFeature _ entF)) = onlyPage (Left entF)
onlyPage (Right (NeighborSourceScaleFeature _ entF)) = onlyPage (Left entF)
onlyPage x = nothingElseButAggr x

onlyTitleAndSectionPath :: CombinedFeature -> Bool
onlyTitleAndSectionPath  (Left (EntRetrievalFeature (GridRun' (GridRun Title _ _ _)) _)) = True
onlyTitleAndSectionPath  (Left (EntRetrievalFeature (GridRun' (GridRun GridFeatures.SectionPath _ _ _)) _)) = True
onlyTitleAndSectionPath  (Right (EdgeRetrievalFeature _ (GridRun' (GridRun Title _ _ _)) _)) = True
onlyTitleAndSectionPath  (Right (EdgeRetrievalFeature _ (GridRun' (GridRun GridFeatures.SectionPath _ _ _)) _)) = True
onlyTitleAndSectionPath (Left (EntRetrievalFeature Aggr runf)) = True
onlyTitleAndSectionPath (Right (EdgeRetrievalFeature _ Aggr runf)) = True
onlyTitleAndSectionPath (Right (NeighborFeature entF)) = onlyTitleAndSectionPath (Left entF)
onlyTitleAndSectionPath (Right (NeighborSourceFeature _ entF)) = onlyTitleAndSectionPath (Left entF)
onlyTitleAndSectionPath (Right (NeighborSourceScaleFeature _ entF)) = onlyTitleAndSectionPath (Left entF)
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
onlySection (Right (NeighborFeature entF)) = onlySection (Left entF)
onlySection (Right (NeighborSourceFeature _ entF)) = onlySection (Left entF)
onlySection (Right (NeighborSourceScaleFeature _ entF)) = onlySection (Left entF)
onlySection x = nothingElseButAggr x



noEdgesFromParas :: CombinedFeature -> Bool
noEdgesFromParas (Left _) = True
noEdgesFromParas (Right (EdgeRetrievalFeature s (_) _)) = not $ (isBaseSource SPara s)
noEdgesFromParas (Right (NeighborSourceFeature s entF)) = not $ (isBaseSource SPara s)
noEdgesFromParas (Right (NeighborSourceScaleFeature s entF)) = not $ (isBaseSource SPara s)
noEdgesFromParas x = nothingElseButAggr x

noEdgesFromAspects :: CombinedFeature -> Bool
noEdgesFromAspects (Left _) = True
noEdgesFromAspects (Right (EdgeRetrievalFeature s (_) _)) = not $ (isBaseSource SAspect s)
noEdgesFromAspects (Right (NeighborSourceFeature s entF)) = not $ (isBaseSource SAspect s)
noEdgesFromAspects (Right (NeighborSourceScaleFeature s entF)) = not $ (isBaseSource SAspect s)
noEdgesFromAspects x = nothingElseButAggr x

noEdgesFromPages :: CombinedFeature -> Bool
noEdgesFromPages (Left _) = True
noEdgesFromPages (Right (EdgeRetrievalFeature s (_) _)) = not $ (isBaseSource SPage s)
noEdgesFromPages (Right (NeighborSourceFeature s entF)) = not $ (isBaseSource SPage s)
noEdgesFromPages (Right (NeighborSourceScaleFeature s entF)) = not $ (isBaseSource SPage s)
noEdgesFromPages x = nothingElseButAggr x

noEdgesFromLinkLink :: CombinedFeature -> Bool
noEdgesFromLinkLink (Left _) = True
noEdgesFromLinkLink (Right (EdgeRetrievalFeature FromAspectsLinkLink (_) _)) = False
noEdgesFromLinkLink (Right (EdgeRetrievalFeature FromPagesLinkLink (_) _)) = False
noEdgesFromLinkLink (Right (NeighborSourceFeature FromPagesLinkLink entF)) = False
noEdgesFromLinkLink (Right (NeighborSourceFeature FromAspectsLinkLink entF)) = False
noEdgesFromLinkLink (Right (NeighborSourceScaleFeature FromPagesLinkLink entF)) = False
noEdgesFromLinkLink (Right (NeighborSourceScaleFeature FromAspectsLinkLink entF)) = False
noEdgesFromLinkLink (Right (EdgeRetrievalFeature _ (_) _)) = True
noEdgesFromLinkLink x = nothingElseButAggr x

data SourceBase = SPage | SAspect | SPara
    deriving Eq
isBaseSource :: SourceBase -> FromSource -> Bool
isBaseSource src FromPagesLinkOwner = src == SPage
isBaseSource src FromPagesOwnerLink = src == SPage
isBaseSource src FromPagesLinkLink = src == SPage
isBaseSource src FromPagesSelf = src == SPage
isBaseSource src FromAspectsLinkOwner = src == SAspect
isBaseSource src FromAspectsOwnerLink = src == SAspect
isBaseSource src FromAspectsLinkLink = src == SAspect
isBaseSource src FromAspectsSelf = src == SAspect
isBaseSource src FromParas = src == SPara


nothingElseButAggr :: CombinedFeature -> Bool
nothingElseButAggr (Left (EntRetrievalFeature Aggr _)) = True
nothingElseButAggr (Left (EntDegree)) = True
nothingElseButAggr (Right (EdgeCount _ )) = True
nothingElseButAggr (Right (EdgeRetrievalFeature _ Aggr _)) = True
nothingElseButAggr (Right (NeighborFeature entF)) = nothingElseButAggr (Left entF)
nothingElseButAggr (Right (NeighborSourceFeature _ entF)) = nothingElseButAggr (Left entF)
nothingElseButAggr (Right (NeighborSourceScaleFeature _ entF)) = nothingElseButAggr (Left entF)
nothingElseButAggr _ = False


onlyExpEcmTestFeature :: CombinedFeature -> Bool
onlyExpEcmTestFeature (Left (EntRetrievalFeature (GridRun' (GridRun Title Bm25 EcmX ParagraphIdx)) ScoreF)) = True
onlyExpEcmTestFeature (Right (EdgeRetrievalFeature _ (GridRun' (GridRun Title Bm25 NoneX ParagraphIdx)) ScoreF)) = True
onlyExpEcmTestFeature (Right (NeighborSourceFeature _ entF)) = onlyExpEcmTestFeature (Left entF)
onlyExpEcmTestFeature (Right (NeighborSourceScaleFeature _ entF)) = onlyExpEcmTestFeature (Left entF)
onlyExpEcmTestFeature (Right (NeighborFeature entF)) = onlyExpEcmTestFeature (Left entF)
onlyExpEcmTestFeature _ = False

onlyNoneX :: CombinedFeature -> Bool
onlyNoneX (Left (EntRetrievalFeature (GridRun' (GridRun Title Bm25 NoneX _)) ScoreF)) = True
onlyNoneX (Right (EdgeRetrievalFeature _ (GridRun' (GridRun Title Bm25 NoneX _)) ScoreF)) = True
onlyNoneX (Right (NeighborFeature entF)) = onlyNoneX (Left entF)
onlyNoneX (Right (NeighborSourceFeature _ entF)) = onlyNoneX (Left entF)
onlyNoneX (Right (NeighborSourceScaleFeature _ entF)) = onlyNoneX (Left entF)
onlyNoneX _ = False

onlySourceNeighbors :: CombinedFeature -> Bool
onlySourceNeighbors  (Right (NeighborSourceFeature _ _)) = True
onlySourceNeighbors  (Right (NeighborFeature _)) = False
onlySourceNeighbors  (Right (NeighborSourceScaleFeature _ _)) = False
onlySourceNeighbors _ = True

onlyScaledSourceNeighbors :: CombinedFeature -> Bool
onlyScaledSourceNeighbors  (Right (NeighborSourceFeature _ _)) = False
onlyScaledSourceNeighbors  (Right (NeighborFeature _)) = False
onlyScaledSourceNeighbors  (Right (NeighborSourceScaleFeature _ _)) = True
onlyScaledSourceNeighbors _ = True

onlyUnsourcedNeighbors :: CombinedFeature -> Bool
onlyUnsourcedNeighbors  (Right (NeighborSourceFeature _ _)) = False
onlyUnsourcedNeighbors  (Right (NeighborFeature _)) = True
onlyUnsourcedNeighbors  (Right (NeighborSourceScaleFeature _ _)) = False
onlyUnsourcedNeighbors _ = True


onlyGraex3 :: CombinedFeature -> Bool
onlyGraex3 (Left (EntRetrievalFeature (GridRun' (GridRun Title Bm25 ex idx)) rmodel )) =
    (rmodel == ScoreF || rmodel == RecipRankF )
    && (idx == EntityIdx || idx == PageIdx || idx == ParagraphIdx)
    && (ex == NoneX || ex == Rm || ex == EcmX || ex == EcmPsg)
onlyGraex3 (Right (EdgeRetrievalFeature FromParas (GridRun' (GridRun Title Bm25 ex idx)) rmodel)) =
    (rmodel == ScoreF || rmodel == RecipRankF )
    && (idx == ParagraphIdx)
    && (ex == NoneX || ex == Rm || ex == EcmPsg)
onlyGraex3 (Left (EntRetrievalFeature Aggr rmodel)) = (rmodel == ScoreF || rmodel == RecipRankF )
onlyGraex3 (Right (EdgeRetrievalFeature FromParas Aggr rmodel)) = (rmodel == ScoreF || rmodel == RecipRankF )
onlyGraex _ = False




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
--       EntIncidentEdgeDocsRecip -> 0.0
--       EntDegreeRecip -> 0.0
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
--       EdgeDocKL _ -> 0
      EdgeRetrievalFeature _ _ runF -> defaultRankFeatures runF
      NeighborFeature x -> defaultEntityFeatures x
      NeighborSourceFeature _ x -> defaultEntityFeatures x
      NeighborSourceScaleFeature _ x -> defaultEntityFeatures x

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



entityScoreVecFromMultiRankings :: MultiRankingEntry PageId GridRun -> [MultiRankingEntry AspectId GridRun] -> [(EntityFeature, Double)]
entityScoreVecFromMultiRankings entityRankEntry aspectRankEntries =
      rankEntFeatures Aggr (multiRankingEntryCollapsed entityRankEntry)
      ++ (featuresFromRankEntry entityRankEntry)
      ++ (entityFeaturesFromAspects aspectRankEntries)
  where
     featuresFromRankEntry entityRankEntry =
        concat [ rankEntFeatures (GridRun' g) entry
                | (g, entry) <- multiRankingEntryAll entityRankEntry
                ]

     entityFeaturesFromAspects entries =
        M.toList $ M.fromListWith (+)  -- multiple features with the same name? add their values!
        $ concat $ fmap featuresFromRankEntry aspectRankEntries

