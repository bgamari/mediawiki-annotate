import Control.DeepSeq
import Data.Either
import Data.Maybe

import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.ByteString.Lazy.Char8 as BSL
import System.FilePath.Glob

import SimplIR.TREC.News as TREC
import SimplIR.TREC as TREC
import WapoCorpus as Wapo
import CAR.Types as CAR
import CAR.Types.Provenance (invalidProvenance)

type Url = T.Text

main :: IO ()
main = do
    let queryFile =  "/home/ben/trec-car/data/wapo/newsir18-entity-ranking-topics.xml"
    topics <- TREC.parseMany TREC.newsQuery <$> TLIO.readFile queryFile
    print topics

    articlePaths <- glob "/home/ben/trec-car/data/wapo/WashingtonPost/data/*.txt"
    articles <- concat <$> mapM readArticles articlePaths

    let (errors, articles') = partitionEithers articles
    print errors

    let articleMap :: HM.HashMap Url Article
        articleMap = HM.fromList [ (articleUrl a, a) | a <- articles' ]

    let (errors', queries') = partitionEithers
            [ case HM.lookup (topicUrl topic) articleMap of
                Just article ->
                    let metadata = mentionMetadata $ map entityId (topicEntities topic)
                    in Right $ (queryToPage article) { pageMetadata = metadata }
                Nothing -> Left (topicNumber topic, topicUrl topic)
            | topic <- topics
            ]

    print errors'
    writeCarFile "queries.cbor" invalidProvenance queries'

readArticles :: FilePath -> IO [Either String Article]
readArticles articlePath = do
    putStrLn articlePath
    xs <- map Aeson.eitherDecode . BSL.lines <$> BSL.readFile articlePath
    return $! rnf xs
    print $ length xs
    return xs

mentionMetadata :: [T.Text] -> PageMetadata
mentionMetadata entities =
     setMetadata _InlinkIds (map (packPageId . T.unpack) entities) emptyPageMetadata

queryToPage :: Article -> CAR.Page
queryToPage a =
    CAR.Page { pageName = packPageName $ T.unpack $ getArticleId $ articleId a
             , pageId = packPageId $ T.unpack $ articleUrl a
             , pageType = ArticlePage
             , pageMetadata = emptyPageMetadata
             , pageSkeleton = mapMaybe toSkel (articleContents a)
             }

  where
    toSkel (Wapo.Title x) = Just $ mkPara x
    toSkel (Wapo.SanitizedParagraph x) = Just $ mkPara x
    toSkel (Wapo.SanitizedSubheading x) = Just $ mkPara x
    toSkel _ = Nothing

    mkPara x = CAR.Para $ CAR.Paragraph paraId [CAR.ParaText x]
    paraId = packParagraphId "deadbeef"
