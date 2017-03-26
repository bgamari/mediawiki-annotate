{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module PassageViewHtml where

import Control.Monad
import Data.Monoid
import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!), p, ul, li, toHtml)
import Text.Blaze.Html5.Attributes as HA
import qualified Text.Blaze.Html.Renderer.Utf8 as H
import qualified Data.ByteString.Lazy as BSL

import CAR.Utils
import CAR.Types
import qualified SimplIR.Format.TrecRunFile as Run


-- | Binary relevance judgement
data IsRelevant = NotRelevant | Relevant
                deriving (Ord, Eq, Show)


data SectionPathWithName = SectionPathWithName { sprQueryId      :: SectionPath
                                               , sprPageName     :: PageName
                                               , sprHeadingPath  :: [SectionHeading]
                                               }

-- data SectionPathResults = SectionPathResults { sprQueryId      :: SectionPath
--                                              , sprPageName     :: PageName
--                                              , sprHeadingPath  :: [SectionHeading]
--                                              , sprRanking      :: [RankingEntry]
--                                              }

data RankingEntry = RankingEntry { entryParagraph :: Paragraph
                                 , entryScore     :: Run.Score
                                 }


passageRankingToHtml :: SectionPathWithName -> [RankingEntry] -> H.Html
passageRankingToHtml SectionPathWithName {..} sprRanking = H.docTypeHtml $ do
    H.head prologue
    H.body $ do
      H.div ! HA.class_ "overview-heading-query" $ do
        H.h1 $ "Query "
            <> toHtml (T.intercalate " // "
                       (getPageName sprPageName : map getSectionHeading sprHeadingPath)
                       )
        H.p $ " (" <> toHtml (escapeSectionPath sprQueryId) <> ") "

      H.div $ do
        H.p $ do
              "Back to "
              H.a ! HA.href ".." $ "Query List"

        H.p ! HA.class_ "entity-snippet-intro" $ "Select relevant / non-relevant paragraphs for this section."

        H.div ! HA.class_ "overview-wide" ! HA.class_ "overview-entities" $ do
            H.h1 "Paragraphs"
            H.ol $ mapM_ renderHtml sprRanking
              where
                renderHtml RankingEntry {..} =
                    paragraphToAnnotationHtml queryId entryParagraph Nothing  paragraphToHtml
                  where queryId = sectionPathToQueryId sprQueryId


paragraphToAnnotationHtml :: QueryId -> Paragraph -> Maybe IsRelevant -> (Paragraph-> H.Html) -> H.Html
paragraphToAnnotationHtml queryId p groundTruthLabel contentHtml =
    H.li ! HA.class_ "entity-snippet-li" $ do
        H.p $ do
            -- H.span ! HA.class_ "htmlscore" $ H.toHtml (show $ entryScore e)
            annotationControl queryId itemId groundTruthLabel
            H.span ! HA.class_ "entity-snippet-li-text" $ do      -- overview-snippets
                    contentHtml p
                    --paragraphToHtml p
  where
    itemId = paragraphToItemId p

-- | A client-side query ID.
newtype QueryId = QueryId H.AttributeValue

sectionPathToQueryId :: SectionPath -> QueryId
sectionPathToQueryId = QueryId . H.stringValue . escapeSectionPath

newtype ItemId = ItemId H.AttributeValue

paragraphToItemId :: Paragraph -> ItemId
paragraphToItemId = ItemId . H.stringValue . unpackParagraphId . paraId

annotationControl :: QueryId -> ItemId -> Maybe IsRelevant -> H.Html
annotationControl (QueryId queryId) (ItemId item) groundTruthLabel =
    H.span ! H.dataAttribute "item"  item ! H.dataAttribute "query" queryId ! annotationClass $ mempty
  where
    annotationClass =  HA.class_ ("annotation " <> groundTruthHtml)
    groundTruthHtml =
      case groundTruthLabel of
        Just Relevant -> "poslabel"
        Just NotRelevant -> "neglabel"
        Nothing -> ""


paragraphToHtml :: Paragraph -> H.Html
paragraphToHtml p = foldMap paraBodyToHtml (paraBody p)

paraBodyToHtml :: ParaBody -> H.Html
paraBodyToHtml (ParaText t) = H.text t
paraBodyToHtml (ParaLink l) = wikipediaLink l $ H.toHtml $ linkAnchor l

wikipediaLink :: Link -> H.Html -> H.Html
wikipediaLink l body =
    H.a ! HA.href url $ body
  where
    PageName n = linkTarget l
    url = H.textValue $ "https://wikipedia.org/wiki/"<>n<>section'
    section' = case linkSection l of
                 Just sect -> "#"<>sect
                 Nothing   -> ""

prologue :: H.Html
prologue = do
    H.meta ! HA.charset "utf-8"
    H.script ! HA.src "http://code.jquery.com/jquery-1.11.0.min.js" $ mempty
    H.script ! HA.src "/annotate.js" $ mempty
    H.link ! HA.rel "stylesheet" ! HA.type_ "text/css" ! HA.href "http://smart-cactus.org/~dietz/knowledgeportfolios/css/bootstrap.min.css"
    H.link ! HA.rel "stylesheet" ! HA.type_ "text/css" ! HA.href "http://smart-cactus.org/~dietz/knowledgeportfolios/annotate.css"
    H.link ! HA.rel "stylesheet" ! HA.type_ "text/css" ! HA.href "http://smart-cactus.org/~dietz/knowledgeportfolios/qipidia.css"
