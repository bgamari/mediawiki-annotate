{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module TQA where

import Control.Applicative
import GHC.Generics
import Codec.Serialise
import Data.Text (Text)
import Data.Aeson
import Data.Hashable
import qualified Data.HashMap.Strict as HM

newtype LessonId = LessonId { getLessonId :: Text }
                 deriving stock (Show, Eq, Ord, Generic)
                 deriving newtype (FromJSON, ToJSON, FromJSONKey, ToJSONKey, Hashable)
                 deriving anyclass (Serialise)

data Lesson = Lesson { lessonGlobalId :: LessonId
                     , lessonName :: Text
                     , lessonTopics :: HM.HashMap TopicId Topic
                     , adjunctTopic :: HM.HashMap Text AdjunctTopic
                     , questions :: HM.HashMap Text QuestionChoice
                     }
            deriving (Show)



instance FromJSON Lesson where
    parseJSON = withObject "lesson" $ \o ->
      Lesson <$> o .: "globalID"
             <*> o .: "lessonName"
             <*> o .: "topics"
             <*> o .: "adjunctTopics"
             <*> ((o .: "questions") >>= (.: "nonDiagramQuestions"))

newtype TopicId = TopicId { getTopicId :: Text }
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

data AdjunctTopic = AdjunctTopic { adjunctTopicText :: Text
                                 }
                  | VocabularyTopic { vocabulary :: HM.HashMap Text Text }
           deriving (Show)

instance FromJSON AdjunctTopic where
    parseJSON v = adjunctTopic v <|> vocabularyTopic v
      where
        adjunctTopic = withObject "adjunctTopic" $ \o ->
          AdjunctTopic <$> ((o .: "content") >>= (.: "text"))
        vocabularyTopic v = VocabularyTopic <$> parseJSON v


newtype QuestionId = QuestionId { getQuestionId :: Text }
                deriving stock (Show, Eq, Ord, Generic)
                deriving newtype (FromJSON, ToJSON, FromJSONKey, ToJSONKey, Hashable)
                deriving anyclass (Serialise)

data QuestionSubType = QuestionTrueOrFalse | QuestionMatching | QuestionMultipleChoice
                deriving (Show, Eq)
instance FromJSON QuestionSubType where
    parseJSON = withText "questionSubType" $ \t ->
      case t of
        "True or False" -> return QuestionTrueOrFalse
        "Multiple Choice" -> return QuestionMultipleChoice
        "Matching" -> return QuestionMatching

newtype QuestionChoice = QuestionChoice { getQuestionChoice :: Text }
                deriving stock (Show, Eq, Ord, Generic)
                deriving anyclass (Serialise)
instance FromJSON QuestionChoice where
    parseJSON = withObject "answerChoices" $ \o ->
      QuestionChoice <$> o .: "processedText"


data NonDiagramQuestion = NonDiagramQuestion { questionId :: QuestionId
                                             , beingAsked :: Text
                                             , answerChoices :: HM.HashMap Text QuestionChoice
                                             , questionSubType :: QuestionSubType
                                             }

instance FromJSON NonDiagramQuestion where
    parseJSON = withObject "nonDiagramQuestion" $ \o ->
      NonDiagramQuestion <$> o .: "globalID"
                         <*>  ((o .: "beingAsked") >>= (.: "processedText"))
                         <*> o .: "answerChoices"  -- todo
                         <*> o .: "questionSubType"



lessonIntroduction :: Lesson -> Maybe AdjunctTopic
lessonIntroduction (Lesson{adjunctTopic=m}) =
    "Introduction" `HM.lookup` m

lessonSummary :: Lesson -> Maybe AdjunctTopic
lessonSummary (Lesson{adjunctTopic=m}) =
    "Lesson Summary" `HM.lookup` m

lessonPoints :: Lesson -> Maybe AdjunctTopic
lessonPoints (Lesson{adjunctTopic=m}) =
    "Points to Consider" `HM.lookup` m

lessonObjectives :: Lesson -> Maybe AdjunctTopic
lessonObjectives (Lesson{adjunctTopic=m}) =
    "Lesson Objectives" `HM.lookup` m

lessonConcepts :: Lesson -> Maybe AdjunctTopic
lessonConcepts (Lesson{adjunctTopic=m}) =
    "Apply Concepts" `HM.lookup` m

lessonRecall :: Lesson -> Maybe AdjunctTopic
lessonRecall (Lesson{adjunctTopic=m}) =
    "Recall" `HM.lookup` m

lessonThink :: Lesson -> Maybe AdjunctTopic
lessonThink (Lesson{adjunctTopic=m}) =
    "Think Critically" `HM.lookup` m

lessonVocabulary :: Lesson -> Maybe AdjunctTopic
lessonVocabulary (Lesson{adjunctTopic=m}) =
    "Vocabulary" `HM.lookup` m