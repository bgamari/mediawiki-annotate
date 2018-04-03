{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module TQA where

import GHC.Generics
import Codec.Serialise
import Data.Text (Text)
import Data.Aeson
import Data.Hashable
import qualified Data.HashMap.Strict as HM

newtype LessonId = LessonId Text
                 deriving stock (Show, Eq, Ord, Generic)
                 deriving newtype (FromJSON, ToJSON, FromJSONKey, ToJSONKey, Hashable)
                 deriving anyclass (Serialise)

data Lesson = Lesson { lessonGlobalId :: LessonId
                     , lessonName :: Text
                     , lessonTopics :: HM.HashMap TopicId Topic
                     }
            deriving (Show)

instance FromJSON Lesson where
    parseJSON = withObject "lesson" $ \o ->
      Lesson <$> o .: "globalID"
             <*> o .: "lessonName"
             <*> o .: "topics"

newtype TopicId = TopicId Text
                deriving stock (Show, Eq, Ord, Generic)
                deriving newtype (FromJSON, ToJSON, FromJSONKey, ToJSONKey, Hashable)
                deriving anyclass (Serialise)

data Topic = Topic { topicId   :: TopicId
                   , topicName :: Text
                   , topicText :: Text
                   }
           deriving (Show)

instance FromJSON Topic where
    parseJSON = withObject "topic" $ \o ->
      Topic <$> o .: "globalID"
            <*> o .: "topicName"
            <*> ((o .: "content") >>= (.: "text"))
