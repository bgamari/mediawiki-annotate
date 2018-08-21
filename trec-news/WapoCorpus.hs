{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module WapoCorpus where

import GHC.Generics
import Control.DeepSeq
import qualified Data.Text as T
import Data.Aeson

newtype ArticleId = ArticleId { getArticleId :: T.Text }
                  deriving (Show, FromJSON, NFData)

data Article = Article { articleId       :: !ArticleId
                       , articleUrl      :: !T.Text
                       , articleAuthor   :: !T.Text
                       , articlePubDate  :: !Int
                       , articleContents :: [Content]
                       }
             deriving (Show, Generic)
instance NFData Article

data Content = KickerText !T.Text
             | Title !T.Text
             | SanitizedParagraph !T.Text
               -- ^ Sanitized paragraph HTML
             | SanitizedSubheading !T.Text
             | OtherContent
             deriving (Show, Generic)
instance NFData Content

instance FromJSON Article where
    parseJSON = withObject "article" $ \o -> do
        Article
            <$> o .: "id"
            <*> o .: "article_url"
            <*> o .: "author"
            <*> o .: "published_date"
            <*> o .: "contents"

instance FromJSON Content where
    parseJSON = withObject "content" $ \o -> do
        ty <- o .: "type"
        case ty :: T.Text of
          "kicker" -> do
              KickerText <$> o .: "content"
          "title" -> do
              Title <$> o .: "content"
          "sanitized_html" -> do
              subty <- o .: "subtype"
              case subty :: T.Text of
                "paragraph" -> SanitizedParagraph <$> o .: "content"
                "subhead" -> SanitizedSubheading <$> o .: "content"
                _ -> return OtherContent
          _ -> return OtherContent
