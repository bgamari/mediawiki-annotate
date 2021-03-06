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
-- import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M

newtype LessonId = LessonId { getLessonId :: Text }
                 deriving stock (Show, Eq, Ord, Generic)
                 deriving newtype (FromJSON, ToJSON, FromJSONKey, ToJSONKey, Hashable)
                 deriving anyclass (Serialise)

data Lesson = Lesson { lessonGlobalId :: LessonId
                     , lessonName :: Text
                     , lessonTopics :: M.Map TopicId Topic
                     , adjunctTopic :: M.Map Text AdjunctTopic
                     , lessonQuestions :: M.Map Text NonDiagramQuestion
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
                  | VocabularyTopic { vocabulary :: M.Map Text Text }
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
                                             , answerChoices :: M.Map Text QuestionChoice
                                             , correctAnswer :: QuestionChoice
                                             , questionSubType :: QuestionSubType
                                             }
  deriving (Show)

instance FromJSON NonDiagramQuestion where
    parseJSON = withObject "nonDiagramQuestion" $ \o ->
      NonDiagramQuestion <$> o .: "globalID"
                         <*>  ((o .: "beingAsked") >>= (.: "processedText"))
                         <*> o .: "answerChoices"
                         <*> o .: "correctAnswer"
                         <*> o .: "questionSubType"



lessonIntroduction :: Lesson -> Maybe AdjunctTopic
lessonIntroduction (Lesson{adjunctTopic=m}) =
    "Introduction" `M.lookup` m

lessonSummary :: Lesson -> Maybe AdjunctTopic
lessonSummary (Lesson{adjunctTopic=m}) =
    case "Lesson Summary" `M.lookup` m of
    Just x -> Just x
    Nothing ->  "Summary" `M.lookup` m

lessonPoints :: Lesson -> Maybe AdjunctTopic
lessonPoints (Lesson{adjunctTopic=m}) =
    "Points to Consider" `M.lookup` m

lessonObjectives :: Lesson -> Maybe AdjunctTopic
lessonObjectives (Lesson{adjunctTopic=m}) =
    "Lesson Objectives" `M.lookup` m

lessonConcepts :: Lesson -> Maybe AdjunctTopic
lessonConcepts (Lesson{adjunctTopic=m}) =
    "Apply Concepts" `M.lookup` m

lessonRecall :: Lesson -> Maybe AdjunctTopic
lessonRecall (Lesson{adjunctTopic=m}) =
    "Recall" `M.lookup` m

lessonThink :: Lesson -> Maybe AdjunctTopic
lessonThink (Lesson{adjunctTopic=m}) =
    "Think Critically" `M.lookup` m

lessonVocabulary :: Lesson -> Maybe AdjunctTopic
lessonVocabulary (Lesson{adjunctTopic=m}) =
    "Vocabulary" `M.lookup` m

----


lessonReview :: Lesson -> Maybe AdjunctTopic
lessonReview (Lesson{adjunctTopic=m}) =
    "Review" `M.lookup` m