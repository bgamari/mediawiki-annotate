{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Types where

import GHC.Generics
import Data.Aeson
import qualified Data.Text as T

import CAR.Types

newtype QueryId = QueryId { unQueryId :: T.Text }
                deriving (Eq, Ord, Show, FromJSON, ToJSON)

jsonOptions :: Options
jsonOptions = defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 2}     -- drop "ap" from field accessor

data AssessmentPage =
    AssessmentPage {
        apTitle :: T.Text,
        apRunId :: T.Text,
        apSquid :: QueryId,
        apQueryFacets :: [AssessmentFacet],
        apParagraphs :: [Paragraph]
    }
  deriving (Show, Generic)
instance FromJSON AssessmentPage where
    parseJSON = genericParseJSON jsonOptions
instance ToJSON AssessmentPage where
    toEncoding = genericToEncoding jsonOptions

data AssessmentFacet =
    AssessmentFacet {
        apHeading :: SectionHeading,
        apHeadingId :: HeadingId
    }
  deriving (Show, Generic)
instance FromJSON AssessmentFacet where
    parseJSON = genericParseJSON jsonOptions
instance ToJSON AssessmentFacet where
    toEncoding = genericToEncoding jsonOptions

