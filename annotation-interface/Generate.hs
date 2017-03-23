{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid
import CAR.Types
import System.FilePath
import qualified SimplIR.Format.TrecRunFile as Run
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Text.Blaze.Html.Renderer.Utf8 as H
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import           Text.Blaze.Html5 ((!))

data Query = Query { queryId      :: HeadingId
                   , queryText    :: SectionPath
                   , queryRanking :: [RankingEntry]
                   }

data RankingEntry = RankingEntry { entryParagraph :: Paragraph
                                 , entryScore     :: Run.Score
                                 }
main ::  IO ()
main = do
    ranking <- Run.readRunFile "run"
    queries <- undefined
    makeAnnotationInterface "." queries

makeAnnotationInterface :: FilePath -> [Query] -> IO ()
makeAnnotationInterface dest queries =
    mapM_ (writeQuery dest) queries

writeQuery :: FilePath -> Query -> IO ()
writeQuery dest query = do
    BSL.writeFile (dest </> unpackHeadingId (queryId query) </> "index.html")
        $ H.renderHtml $ queryToHtml query

queryToHtml :: Query -> H.Html
queryToHtml q = H.docTypeHtml $ do
    H.head $ do
        prologue
        H.title $ H.toHtml $ "Query: " <> qid
    H.body $ do
        H.h1 ! HA.class_ "entity-heading" $ H.toHtml qid
        H.p $ "Query ranking for "<>H.toHtml qid<>", "<>query
        H.ol $ foldMap rankingEntryToHtml (queryRanking q)
  where
    query = foldMap (H.toHtml . unpackHeadingId) (sectionPathHeadings $ queryText q)
    qid = unpackHeadingId $ queryId q
    dataQuery = H.dataAttribute "query" $ H.stringValue $ unpackHeadingId $ queryId q

    rankingEntryToHtml :: RankingEntry -> H.Html
    rankingEntryToHtml e = H.li ! HA.class_ "entity-snippet-li" $ do
        H.p $ do
            H.span ! HA.class_ "htmlscore" $ H.toHtml (show $ entryScore e)
            H.span ! dataItem ! dataQuery ! HA.class_ "annotation neglabel" $ do
                H.span ! HA.class_ "entity-snippet-li-text" $ do
                    foldMap paraBodyToHtml (paraBody p)
      where
        p = entryParagraph e
        dataItem = H.dataAttribute "item" (H.stringValue $ unpackParagraphId $ paraId p)

paraBodyToHtml :: ParaBody -> H.Html
paraBodyToHtml (ParaText t) = H.text t
paraBodyToHtml (ParaLink l) = H.a ! HA.href target $ H.toHtml $ linkAnchor l
  where
    target = H.textValue $ getPageName (linkTarget l) <> section
    section = case linkSection l of
                Just sect -> "#"<>sect
                Nothing   -> ""

wikipediaLink :: PageName -> H.Html -> H.Html
wikipediaLink (PageName n) body =
    H.a ! HA.href url $ body
  where url = H.textValue $ "https://wikipedia.org/wiki/"<>n

prologue :: H.Html
prologue = do
    H.meta ! HA.charset "utf-8"
    H.script ! HA.src "http://code.jquery.com/jquery-1.11.0.min.js" $ mempty
    H.script ! HA.src "/annotate.js" $ mempty
    H.link ! HA.rel "stylesheet" ! HA.type_ "text/css" ! HA.href "http://smart-cactus.org/~dietz/knowledgeportfolios/css/bootstrap.min.css"
    H.link ! HA.rel "stylesheet" ! HA.type_ "text/css" ! HA.href "http://smart-cactus.org/~dietz/knowledgeportfolios/annotate.css"
    H.link ! HA.rel "stylesheet" ! HA.type_ "text/css" ! HA.href "http://smart-cactus.org/~dietz/knowledgeportfolios/qipidia.css"

