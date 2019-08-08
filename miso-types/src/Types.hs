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
import Data.List
import Control.Applicative
import qualified Data.Text as T
import Data.Hashable
import qualified Data.Map.Strict as M
import qualified Data.Set as S
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
            linkTargetId <- packPageId <$> o .: "entity"
            linkAnchor <- o .: "text"
            linkTarget <- packPageName <$> o .: "entity_name"
            return $ ParaLink $ Link {..}
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


data SubmissionRun =
    SubmissionRun {
        apSubmissionData :: [AssessmentPage]
    }
  deriving (Eq, Show, Generic)
instance FromJSON SubmissionRun where
    parseJSON = genericParseJSON jsonOptions
instance ToJSON SubmissionRun where
    toJSON = genericToJSON jsonOptions
    toEncoding = genericToEncoding jsonOptions

data AssessmentPage =
    AssessmentPage {
        apTitle :: T.Text,
        apRunId :: RunId,
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

type RunId = T.Text

data AssessmentLabel = UnsetLabel | TrashLabel | DuplicateLabel | NonRelLabel | TopicLabel | CanLabel | ShouldLabel | MustLabel
    deriving (Eq, Ord, FromJSON, ToJSON, Generic, Show)

data AssessmentTransitionLabel = UnsetTransition |  ToNonRelTransition | OfftopicTransition | SwitchTransition | CoherentTransition |   AppropriateTransition | SameTransition | RedundantTransition
    deriving (Eq, Ord, FromJSON, ToJSON, Generic, Show)





data AssessmentKey = AssessmentKey {
        queryId :: QueryId
        , paragraphId :: ParagraphId
    }
  deriving (Eq, Hashable, Ord, FromJSONKey, ToJSONKey, Generic, Show)
instance FromJSON AssessmentKey where
    parseJSON = genericParseJSON json2Options
instance ToJSON AssessmentKey where
    toJSON = genericToJSON json2Options
    toEncoding = genericToEncoding json2Options


data AssessmentTransitionKey = AssessmentTransitionKey {
        queryId :: QueryId
        , paragraphId1 :: ParagraphId
        , paragraphId2 :: ParagraphId
    }
  deriving (Eq, Hashable, Ord, FromJSONKey, ToJSONKey, Generic, Show)
instance FromJSON AssessmentTransitionKey where
    parseJSON = genericParseJSON json2Options
instance ToJSON AssessmentTransitionKey where
    toJSON = genericToJSON json2Options
    toEncoding = genericToEncoding json2Options



data AssessmentState = AssessmentState {
                    notesState :: M.Map AssessmentKey [(AnnotationValue T.Text)]
                    , facetState :: M.Map AssessmentKey [(AnnotationValue FacetValue)]
                    , transitionLabelState :: M.Map AssessmentTransitionKey (AnnotationValue AssessmentTransitionLabel)
                    , nonrelevantState :: M.Map AssessmentKey (AnnotationValue ())
                    , nonrelevantState2 :: Maybe (M.Map AssessmentKey (AnnotationValue Bool))
                    , assessorData :: M.Map UserId (AnnotationValue ())
    }
  deriving (Eq, Generic, Show)
instance FromJSON AssessmentState where
    parseJSON = genericParseJSON json2Options
instance ToJSON AssessmentState where
    toJSON = genericToJSON json2Options
    toEncoding = genericToEncoding json2Options

mergeAssessmentState :: AssessmentState -> AssessmentState -> AssessmentState
mergeAssessmentState newState oldState =
    AssessmentState { notesState = M.unionWith unionListsWithTimestamp (notesState newState) (notesState oldState)
                    , facetState = M.unionWith unionListsWithTimestamp (facetState newState) (facetState oldState)
                    , transitionLabelState = M.unionWith unionWithTimestamp (transitionLabelState newState) (transitionLabelState oldState)
                    , nonrelevantState = nonrelevantState newState --  we merge the oldstate into nonrelevantState2
                    , nonrelevantState2 = mergeNonrelevantState (nonrelevantState2 newState) (nonrelevantState2 oldState) (nonrelevantState oldState)
                    , assessorData = M.unionWith mergeAssessorData (assessorData newState) (assessorData oldState)
                    }
  where unionWithTimestamp :: AnnotationValue a -> AnnotationValue a -> AnnotationValue a
        unionWithTimestamp v1@AnnotationValue{timeStamp = time1} v2@AnnotationValue{timeStamp = time2} =
            if time1 < time2 then v2 else v1

        unionListsWithTimestamp :: [AnnotationValue a] -> [AnnotationValue a] -> [AnnotationValue a]
        unionListsWithTimestamp v1@(AnnotationValue{timeStamp = time1}:_) v2@(AnnotationValue{timeStamp = time2}:_) =
            if time1 < time2 then v2 else v1

        mergeAssessorData :: (AnnotationValue ()) -> (AnnotationValue ()) -> (AnnotationValue ())
        mergeAssessorData av@AnnotationValue{runIds=val1} AnnotationValue{runIds=val2} =
            av {runIds = nub (val1 <> val2)}
        convertNonrelevantState ::    M.Map AssessmentKey (AnnotationValue ())  ->  (M.Map AssessmentKey (AnnotationValue Bool))
        convertNonrelevantState old =
            let old2 = M.fromList
                     $ [(key, AnnotationValue {annotatorId = annotatorId
                                              , timeStamp = timeStamp
                                              , sessionId = sessionId
                                              , runIds = runIds
                                              , value = True})
                     | (key, AnnotationValue{..}) <- M.toList old
                     ]
            in old2

        readOrInitializeNonrelevantState :: Maybe (M.Map AssessmentKey (AnnotationValue Bool)) -> (M.Map AssessmentKey (AnnotationValue Bool))
        readOrInitializeNonrelevantState ( Just new2 ) = new2
        readOrInitializeNonrelevantState Nothing = mempty


        mergeNonrelevantState :: Maybe (M.Map AssessmentKey (AnnotationValue Bool))
                              -> Maybe (M.Map AssessmentKey (AnnotationValue Bool))
                              ->  M.Map AssessmentKey (AnnotationValue ())
                              -> Maybe (M.Map AssessmentKey (AnnotationValue Bool))
        mergeNonrelevantState new2 (Just old2) _ = Just $ M.unionWith unionWithTimestamp (readOrInitializeNonrelevantState new2) old2  -- ignore old values, because they were already converted
        mergeNonrelevantState new2 Nothing old | null old  = new2
        mergeNonrelevantState new2 Nothing old =
            Just $ M.unionWith unionWithTimestamp (readOrInitializeNonrelevantState new2) (convertNonrelevantState old)


data AnnotationValue a = AnnotationValue {
    annotatorId :: UserId
    , timeStamp :: UTCTime
    , sessionId :: T.Text
    , runIds :: [T.Text]
    , value :: a
  }
  deriving (Eq, Generic, Show)
instance FromJSON a => FromJSON (AnnotationValue a) where
    parseJSON = genericParseJSON json2Options
instance ToJSON a =>  ToJSON (AnnotationValue a) where
    toJSON = genericToJSON json2Options
    toEncoding = genericToEncoding json2Options


unwrapAnnotationValue :: AnnotationValue a -> a
unwrapAnnotationValue (AnnotationValue {value = v}) = v

unwrapMaybeAnnotationValue :: a -> Maybe (AnnotationValue a) -> a
unwrapMaybeAnnotationValue _defVal (Just (AnnotationValue {value = v})) = v
unwrapMaybeAnnotationValue defVal Nothing = defVal

unwrapMaybeAnnotationValueList :: [a] -> Maybe [(AnnotationValue a)] -> [a]
unwrapMaybeAnnotationValueList _defVal (Just annotationValues) = [v | (AnnotationValue {value = v}) <- annotationValues]
unwrapMaybeAnnotationValueList defVal Nothing = defVal


data FacetValue = FacetValue {
    facet :: AssessmentFacet
    , relevance :: AssessmentLabel

  }
  deriving (Eq, Generic, Show)
instance FromJSON FacetValue where
    parseJSON = genericParseJSON json2Options
instance ToJSON FacetValue where
    toJSON = genericToJSON json2Options
    toEncoding = genericToEncoding json2Options


emptyAssessmentState = AssessmentState { notesState = mempty
                                       , facetState = mempty
                                       , transitionLabelState = mempty
                                       , nonrelevantState = mempty
                                       , nonrelevantState2 = Just mempty
                                       , assessorData = mempty
                                       }

data AssessmentMetaData = AssessmentMetaData {
     runIds :: [T.Text]
   , annotatorIds :: [UserId]
   , timeStamp :: Maybe UTCTime
   , sessionId :: Maybe T.Text
   }
  deriving (Eq, Generic, Show)
instance FromJSON AssessmentMetaData where
    parseJSON = genericParseJSON json2Options
instance ToJSON AssessmentMetaData where
    toJSON = genericToJSON json2Options
    toEncoding = genericToEncoding json2Options



data SavedAssessments = SavedAssessments {
        savedData :: AssessmentState
       , metaData :: AssessmentMetaData
    }
  deriving (Eq, Generic, Show)
instance FromJSON SavedAssessments where
    parseJSON = genericParseJSON json2Options
instance ToJSON SavedAssessments where
    toJSON = genericToJSON json2Options
    toEncoding = genericToEncoding json2Options


