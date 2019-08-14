module MisoPostprocessing.Utils where

import Data.Text as T

import CAR.Types
import Types

import SimplIR.Types.Relevance


squidToQueryId :: QueryId -> PageId
squidToQueryId squid = packPageId $ T.unpack $ unQueryId squid

assessmentLabelToGradedRelevance :: AssessmentLabel -> Maybe GradedRelevance
assessmentLabelToGradedRelevance UnsetLabel = Nothing
assessmentLabelToGradedRelevance TrashLabel = Just $ GradedRelevance (-2)
assessmentLabelToGradedRelevance DuplicateLabel = Nothing
assessmentLabelToGradedRelevance NonRelLabel = Just $ GradedRelevance 0
assessmentLabelToGradedRelevance TopicLabel = Just $ GradedRelevance 0
assessmentLabelToGradedRelevance CanLabel = Just $ GradedRelevance 1
assessmentLabelToGradedRelevance ShouldLabel = Just $ GradedRelevance 2
assessmentLabelToGradedRelevance MustLabel = Just $ GradedRelevance 3
