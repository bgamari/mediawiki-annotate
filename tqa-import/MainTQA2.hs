{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Hashable
import Data.Semigroup ((<>))
import Data.Aeson as Aeson
import Data.Foldable
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL
import Options.Applicative
import qualified Data.HashMap.Strict as HM
import Data.Maybe


import CAR.Types
import TQA

options :: Parser (FilePath, FilePath)
options =
    (,) <$> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output CAR pages file")
        <*> argument str (metavar "FILE" <> help "Input TQA JSON file")

main :: IO ()
main = do
    (outputPath, inputPath) <- execParser $ info (helper <*> options) mempty
    lessons <- either error id . Aeson.eitherDecode <$> BSL.readFile inputPath
    --print $ map lessonToPage lessons
    let siteProv = SiteProvenance { provSiteId = siteId
                                  , language = Language "en-us"
                                  , sourceName = "tqa"
                                  , siteComments = []
                                  }
        prov = Provenance { siteProvenances = [siteProv]
                          , dataReleaseName = "TQA2"
                          , comments = []
                          , transforms = []
                          }
    writeCarFile outputPath prov $ map lessonToPage lessons

siteId :: SiteId
siteId = "tqa2"

lessonToPage :: Lesson -> Page
lessonToPage l =
    Page { pageName = pageName
         , pageId =  packPageId $ T.unpack $ encodeLessonId siteId (lessonGlobalId l)-- pageNameToId siteId pageName
         , pageType = ArticlePage
         , pageMetadata = emptyPageMetadata
         , pageSkeleton = intro <> vocabulary <> sections
         }
  where
    pageName = packPageName $ T.unpack $ lessonName l
    sections = map topicToSection $ toList $ lessonTopics l
    intro = maybe [] (pure . adjunctTopicToSkel) $ lessonIntroduction l
    vocabulary = vocabularyToSkel $ lessonVocabulary l


encodeLessonId :: SiteId -> LessonId -> T.Text
encodeLessonId (SiteId siteId) (LessonId lessonId) =  siteId <> ":" <> lessonId

decodeLessonId :: T.Text -> SiteId -> Maybe LessonId
decodeLessonId text (SiteId siteId)  =
    let (pref,suff) = T.span (==':') text
    in  if pref == siteId then
            Just $ LessonId suff
        else Nothing

topicToSection :: Topic -> PageSkeleton
topicToSection t =
    Section heading headingId [content]
  where
    heading = SectionHeading (topicName t)
--     headingId = sectionHeadingToId heading
    headingId = packHeadingId $ T.unpack $ getTopicId $ topicId t
    content = Para $ Paragraph paraId [ParaText $ topicText t]
      where paraId = packParagraphId $ show $ hash $ topicText t

adjunctTopicToSkel :: AdjunctTopic -> PageSkeleton
adjunctTopicToSkel (AdjunctTopic t) =
    Para $ Paragraph paraId [ParaText t]
      where paraId = packParagraphId $ show $ hash t

adjunctTopicToSkel _ = error ("can only be applied to AdjunctTopic")


vocabularyToSkel :: Maybe AdjunctTopic -> [PageSkeleton]
vocabularyToSkel (Just (VocabularyTopic vocab)) =
    fmap vocabList $ HM.toList vocab
  where vocabList (key,v) =
            List 1 $ Paragraph paraId [ParaText t]
              where paraId = packParagraphId $ show $ hash t
                    t = if T.length v == 0 then
                            key
                        else
                            key <> " (" <> v <> ")"
vocabularyToSkel Nothing =
    []
vocabularyToSkel  _ = error ("can only be applied to VocabularyTopic")


