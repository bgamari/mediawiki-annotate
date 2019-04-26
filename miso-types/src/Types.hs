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
import Data.Time

import CAR.Types

newtype QueryId = QueryId { unQueryId :: T.Text }
                deriving (Eq, Ord, Show)
                deriving newtype (Hashable, FromJSON, ToJSON)

jsonOptions :: Options
jsonOptions = defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 2}     -- drop "ap" from field accessor
json2Options = defaultOptions { fieldLabelModifier = camelTo2 '_' }     -- camel case -> snake case

-- orphans
-- instance FromJSON ParaBody
-- instance FromJSON Paragraph
-- instance FromJSON Link
-- instance ToJSON ParaBody
-- instance ToJSON Paragraph
-- instance ToJSON Link

instance FromJSON Paragraph where
    parseJSON = genericParseJSON json2Options
instance ToJSON Paragraph where
    toJSON = genericToJSON json2Options
    toEncoding = genericToEncoding json2Options

instance FromJSON Link where
    parseJSON = genericParseJSON json2Options
instance ToJSON Link where
    toJSON = genericToJSON json2Options
    toEncoding = genericToEncoding json2Options



instance  FromJSON ParaBody where
    parseJSON v =
        link v <|> text
      where
        link = withObject "ParaLink" $ \o -> do
            linkSection <- o .:? "entity_section"
            linkTargetId <- o .: "entity"
            linkAnchor <- o .: "text"
            linkTarget <- o .: "entity_name"
            ParaLink $ Link {..}
        text = (withObject "ParaText" $ \o -> ParaText <$> o .: "text") v

instance ToJSON ParaBody where
    toJSON (ParaLink Link {..}) =
        let maybeSection =
                case linkSection of
                Nothing -> []
                Just section -> ["entity_section" .= section]
        in object ([ "entity" .= unpackPageId linkTargetId
                   , "text" .= linkAnchor
                   , "entity_name" .= unpackPageName linkTarget
                   ]
                   <> maybeSection
                  )

    toJSON (ParaText txt) = object [ "text" .= txt ]



data AssessmentPage =
    AssessmentPage {
        apTitle :: T.Text,
        apRunId :: T.Text,
        apSquid :: QueryId,
        apQueryFacets :: [AssessmentFacet],
        apParagraphs :: [Paragraph],
        apParagraphAssessments :: Maybe [ParagraphAssessments],
        apParagraphOrigins :: Maybe [ParagraphOrgins]
    }
  deriving (Eq, Show, Generic)
instance FromJSON AssessmentPage where
    parseJSON = genericParseJSON jsonOptions
instance ToJSON AssessmentPage where
    toJSON = genericToJSON jsonOptions
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
    toJSON = genericToJSON jsonOptions
    toEncoding = genericToEncoding jsonOptions

data ParagraphAssessments =
    ParagraphAssessments {
        apParaId :: ParagraphId,
        apFacet :: HeadingId,
        apAssessment :: AssessmentLabel
    }
  deriving (Eq, Show, Generic)
instance FromJSON ParagraphAssessments where
    parseJSON = genericParseJSON jsonOptions
instance ToJSON ParagraphAssessments where
    toJSON = genericToJSON jsonOptions
    toEncoding = genericToEncoding jsonOptions

data ParagraphOrgins =
    ParagraphOrgins {
        apParaId :: ParagraphId,
        apSectionPath ::  T.Text, -- HeadingId,
        apRankScore :: Double,
        apRank :: Int
    }
  deriving (Eq, Show, Generic)
instance FromJSON ParagraphOrgins where
    parseJSON = genericParseJSON jsonOptions
instance ToJSON ParagraphOrgins where
    toJSON = genericToJSON jsonOptions
    toEncoding = genericToEncoding jsonOptions

-- --------------------------------------

type UserId = T.Text


data AssessmentLabel = MustLabel | ShouldLabel | CanLabel | TopicLabel | NonRelLabel | TrashLabel  |DuplicateLabel |UnsetLabel
    deriving (Eq, FromJSON, ToJSON, Generic, Show)

data AssessmentTransitionLabel = RedundantTransition | SameTransition | AppropriateTransition | SwitchTransition | OfftopicTransition | ToNonRelTransition | UnsetTransition
    deriving (Eq, FromJSON, ToJSON, Generic, Show)





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
                    , facetState :: M.Map AssessmentKey [AssessmentFacet]
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

