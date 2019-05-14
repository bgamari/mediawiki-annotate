{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields#-}
{-# LANGUAGE DeriveAnyClass #-}

module OldTypes where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import Control.Applicative
import qualified Data.Text as T
import Data.Hashable
import qualified Data.Map.Strict as M
import Data.Time

import CAR.Types


import Types (QueryId, UserId, AssessmentLabel, AssessmentTransitionLabel)

jsonOptions :: Options
jsonOptions = defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 2}     -- drop "ap" from field accessor
json2Options = defaultOptions { fieldLabelModifier = camelTo2 '_' }     -- camel case -> snake case


data AssessmentFacet =
    AssessmentFacet {
        apHeading :: SectionHeading,
        apHeadingId :: HeadingId
    }
  deriving (Eq, Show, Generic)
instance FromJSON AssessmentFacet where
    parseJSON = genericParseJSON jsonOptions
instance ToJSON AssessmentFacet where
    toJSON = genericToJSON jsonOptions
    toEncoding = genericToEncoding jsonOptions
--
-- data ParagraphAssessments =
--     ParagraphAssessments {
--         apParaId :: ParagraphId,
--         apFacet :: HeadingId,
--         apAssessment :: AssessmentLabel
--     }
--   deriving (Eq, Show, Generic)
-- instance FromJSON ParagraphAssessments where
--     parseJSON = genericParseJSON jsonOptions
-- instance ToJSON ParagraphAssessments where
--     toJSON = genericToJSON jsonOptions
--     toEncoding = genericToEncoding jsonOptions
--
-- data ParagraphOrgins =
--     ParagraphOrgins {
--         apParaId :: ParagraphId,
--         apSectionPath ::  T.Text, -- HeadingId,
--         apRankScore :: Double,
--         apRank :: Int
--     }
--   deriving (Eq, Show, Generic)
-- instance FromJSON ParagraphOrgins where
--     parseJSON = genericParseJSON jsonOptions
-- instance ToJSON ParagraphOrgins where
--     toJSON = genericToJSON jsonOptions
--     toEncoding = genericToEncoding jsonOptions


data AssessmentKey = AssessmentKey {
        userId :: UserId
        , queryId :: QueryId
        , paragraphId :: ParagraphId
    }
  deriving (Eq, Hashable, Ord, FromJSON, ToJSON, FromJSONKey, ToJSONKey, Generic, Show)

data AssessmentTransitionKey = AssessmentTransitionKey {
        userId :: UserId
        , queryId :: QueryId
        , paragraphId1 :: ParagraphId
        , paragraphId2 :: ParagraphId
    }
  deriving (Eq, Hashable, Ord, FromJSON, ToJSON, FromJSONKey, ToJSONKey, Generic, Show)



data AssessmentState = AssessmentState {
                    labelState :: M.Map AssessmentKey AssessmentLabel
                    , notesState :: M.Map AssessmentKey T.Text
                    , facetState :: M.Map AssessmentKey AssessmentFacet
                    , transitionLabelState :: M.Map AssessmentTransitionKey AssessmentTransitionLabel
                    , transitionNotesState :: M.Map AssessmentTransitionKey T.Text
                    , hiddenState :: M.Map AssessmentKey Bool
    }
  deriving (Eq, FromJSON, ToJSON, Generic, Show)

emptyAssessmentState = AssessmentState { labelState = mempty
                                       , notesState = mempty
                                       , facetState = mempty
                                       , transitionLabelState = mempty
                                       , transitionNotesState = mempty
                                       , hiddenState = mempty
                                       }

data AssessmentMetaData = AssessmentMetaData {
     assessmentRuns :: [AssessmentRun]
   , userId :: UserId
   , timeStamp :: UTCTime
   }
  deriving (Eq, FromJSON, ToJSON, Generic, Show)


data AssessmentRun = AssessmentRun {
          runId :: T.Text
        , squid :: QueryId
    }
  deriving (Eq, FromJSON, ToJSON, Generic, Show)

data SavedAssessments = SavedAssessments {
        savedData :: AssessmentState
       , metaData :: AssessmentMetaData
    }
  deriving (Eq, FromJSON, ToJSON, Generic, Show)



