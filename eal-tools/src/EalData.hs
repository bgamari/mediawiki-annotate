{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}


module EalData where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Generics (Generic)

type PageId = T.Text
type LocationId = T.Text
type ParagraphId = T.Text
type SectionId = [T.Text]
type AspectId = T.Text
type EALId = T.Text


type PageTitle = T.Text
type SectionName = T.Text
type AspectName = T.Text

data Location = Location { location_id :: LocationId
                         , page_id :: PageId
                         , page_title :: PageTitle 
                         , paragraph_id :: Maybe ParagraphId
                         , section_id :: SectionId
                         , section_headings :: [SectionName]
                         }
                deriving (Show, Eq, Generic, Aeson.FromJSON, Aeson.ToJSON)

data EntityMention = EntityMention { entity_name :: PageTitle
                                   , entity_id :: PageId
                                   , mention :: T.Text
                                   , target_mention :: Maybe Bool
                                   , start :: Int
                                   , end :: Int
                                   }
                deriving (Show, Eq, Generic, Aeson.FromJSON, Aeson.ToJSON)

data AnnotatedText = AnnotatedText { content :: T.Text
                                   , entities :: [EntityMention]
                                   }
                deriving (Show, Eq, Generic, Aeson.FromJSON, Aeson.ToJSON)


data Context = Context { target_entity :: PageId
                       , location :: Location
                       , sentence :: AnnotatedText
                       , paragraph :: AnnotatedText
                       }
                deriving (Show, Eq, Generic, Aeson.FromJSON, Aeson.ToJSON)

data Aspect = Aspect { aspect_id :: AspectId 
                     , location :: Location
                     , aspect_content :: AnnotatedText
                     , aspect_name :: AspectName
                     }
                deriving (Show, Eq, Generic, Aeson.FromJSON, Aeson.ToJSON)

data AspectLinkExample = AspectLinkExample { unhashed_id :: T.Text
                                           , id :: EALId
                                           , context :: Context
                                           , true_aspect :: AspectId
                                           , candidate_aspects :: [Aspect]
                                           }
                deriving (Show, Eq, Generic, Aeson.FromJSON, Aeson.ToJSON)


readAspectExamples :: FilePath -> IO [Either String AspectLinkExample]
readAspectExamples fname = do
    bs <- BSL.readFile fname
    let ealExamples :: [Either String AspectLinkExample]
        ealExamples =   [ Aeson.eitherDecode line
                        | line <- BSL.lines bs
                        ]
    return $ ealExamples


type AspectLinkExampleOrError = Either String AspectLinkExample