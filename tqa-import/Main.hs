{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Hashable
import Data.Semigroup ((<>))
import Data.Aeson as Aeson
import Data.Foldable
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL
import Options.Applicative

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
                          , dataReleaseName = "TQA"
                          , comments = []
                          , transforms = []
                          }
    writeCarFile outputPath prov $ map lessonToPage lessons

siteId :: SiteId
siteId = "tqa"

lessonToPage :: Lesson -> Page
lessonToPage l =
    Page { pageName = pageName
         , pageId = pageNameToId siteId pageName
         , pageType = ArticlePage
         , pageMetadata = emptyPageMetadata
         , pageSkeleton = map topicToSection $ toList $ lessonTopics l
         }
  where
    pageName = packPageName $ T.unpack $ lessonName l

topicToSection :: Topic -> PageSkeleton
topicToSection t =
    Section heading headingId [content]
  where
    heading = SectionHeading (topicName t)
    headingId = sectionHeadingToId heading
    content = Para $ Paragraph paraId [ParaText $ topicText t]
      where paraId = packParagraphId $ show $ hash headingId


