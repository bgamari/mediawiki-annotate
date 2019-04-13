{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields#-}
{-# LANGUAGE DeriveAnyClass #-}


module Types where


import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import Control.Applicative
import qualified Data.Text as T
import Data.Hashable
import qualified Data.Map.Strict as M

import CAR.Types

newtype QueryId = QueryId { unQueryId :: T.Text }
                deriving (Eq, Ord, Show)
                deriving newtype (Hashable, FromJSON, ToJSON)

jsonOptions :: Options
jsonOptions = defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 2}     -- drop "ap" from field accessor

-- orphans
-- instance FromJSON ParaBody
instance FromJSON Paragraph
instance FromJSON Link
-- instance ToJSON ParaBody
instance ToJSON Paragraph
instance ToJSON Link


instance  FromJSON ParaBody where
    parseJSON v =
        link <|> text
      where
        link = ParaLink <$> parseJSON v
        text = (withObject "ParaText" $ \o -> ParaText <$> o .: "text") v

instance ToJSON ParaBody where
    toJSON = undefined



data AssessmentPage =
    AssessmentPage {
        apTitle :: T.Text,
        apRunId :: T.Text,
        apSquid :: QueryId,
        apQueryFacets :: [AssessmentFacet],
        apParagraphs :: [Paragraph]
    }
  deriving (Eq, Show, Generic)
instance FromJSON AssessmentPage where
    parseJSON = genericParseJSON jsonOptions
instance ToJSON AssessmentPage where
    toEncoding = genericToEncoding jsonOptions

data AssessmentFacet =
    AssessmentFacet {
        apHeading :: SectionHeading,
        apHeadingId :: HeadingId
    }
  deriving (Eq, Show, Generic)
instance FromJSON AssessmentFacet where
    parseJSON = genericParseJSON jsonOptions
instance ToJSON AssessmentFacet where
    toEncoding = genericToEncoding jsonOptions

-- --------------------------------------

type UserId = T.Text
defaultUser :: UserId
defaultUser = "defaultuser"


data AssessmentLabel = MustLabel | ShouldLabel | CanLabel | TopicLabel | NonRelLabel | TrashLabel  |DuplicateLabel |UnsetLabel
    deriving (Eq, FromJSON, ToJSON, Generic, Show)

data AssessmentTransitionLabel = RedundantTransition | SameTransition | AppropriateTransition | SwitchTransition | OfftopicTransition | ToNonRelTransition | UnsetTransition
    deriving (Eq, FromJSON, ToJSON, Generic, Show)





data AssessmentKey = AssessmentKey {
        userId :: UserId
        , queryId :: QueryId
        , paragraphId :: ParagraphId
    }
  deriving (Eq, Hashable, Ord, FromJSON, ToJSON, FromJSONKey, ToJSONKey, Generic)

data AssessmentTransitionKey = AssessmentTransitionKey {
        userId :: UserId
        , queryId :: QueryId
        , paragraphId1 :: ParagraphId
        , paragraphId2 :: ParagraphId
    }
  deriving (Eq, Hashable, Ord, FromJSON, ToJSON, FromJSONKey, ToJSONKey, Generic)



data AssessmentState = AssessmentState {
                    labelState :: M.Map AssessmentKey AssessmentLabel
                    , notesState :: M.Map AssessmentKey T.Text
                    , facetState :: M.Map AssessmentKey AssessmentFacet
                    , transitionLabelState :: M.Map AssessmentTransitionKey AssessmentTransitionLabel
                    , transitionNotesState :: M.Map AssessmentTransitionKey T.Text
                    , hiddenState :: M.Map AssessmentKey Bool
    }
  deriving (Eq, FromJSON, ToJSON, Generic)

emptyAssessmentState = AssessmentState { labelState = mempty
                                       , notesState = mempty
                                       , facetState = mempty
                                       , transitionLabelState = mempty
                                       , transitionNotesState = mempty
                                       , hiddenState = mempty
                                       }

data SavedAssessments = SavedAssessments {
        savedData :: AssessmentState
    }
  deriving (Eq, FromJSON, ToJSON, Generic)

