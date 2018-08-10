{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Hashable
import Data.Semigroup ((<>))
import Data.Aeson as Aeson
import Data.Foldable
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy as BSL
import Options.Applicative
import Data.Char
import Data.Text.IO

import CAR.Types
import TQA

options :: Parser (FilePath, FilePath, T.Text)
options =
    (,,) <$> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output CAR pages file")
         <*> argument str (metavar "FILE" <> help "Input Query file")
         <*> option (T.pack <$> str) (short 'n' <> long "release-name" <> metavar "RELEASE" <> help "Release name")
main :: IO ()
main = do
    (outputPath, inputPath, releaseName) <- execParser $ info (helper <*> options) mempty
    text <- TIO.readFile inputPath
    let queries = fmap splitQueries $ T.lines text
    --print $ map lessonToPage lessons
        siteProv = SiteProvenance { provSiteId = siteId
                                  , language = Language "en-us"
                                  , sourceName = "topicfile"
                                  , siteComments = []
                                  }
        prov = Provenance { siteProvenances = [siteProv]
                          , dataReleaseName = releaseName
                          , comments = []
                          , transforms = []
                          }
    writeCarFile outputPath prov $ map queriesToPage queries

siteId :: SiteId
siteId = "query"

data Query = Query { queryId :: T.Text
                   , queryText :: T.Text
                   }
                   deriving Show


splitQueries :: T.Text -> Query
splitQueries line =
    let (queryId, queryText) = T.span (not . isSpace) line
        queryId' = T.strip queryId
        queryText' = T.strip queryText
    in Query queryId' queryText'



queriesToPage :: Query -> Page
queriesToPage Query{..} =
    Page { pageName = pageName
         , pageId = packPageId $ T.unpack queryId
         , pageType = ArticlePage
         , pageMetadata = emptyPageMetadata
         , pageSkeleton = mempty
         }
  where
    pageName = packPageName $ T.unpack queryText

