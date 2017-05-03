{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module TrecCarRenderHtml where

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

import qualified SimplIR.Format.TrecRunFile as TrecRun


data RankingEntry = RankingEntry { entryParagraph :: Paragraph
                                 , entryScore     :: TrecRun.Score
                                 }





data SectionPathWithName = SectionPathWithName { sprQueryId      :: SectionPath
                                               , sprPageName     :: PageName
                                               , sprHeadingPath  :: [SectionHeading]
                                               }


-- ========= Annotation Control, does not care what it is annotating   =============

annotationControl :: AnnotationQueryId -> ItemId -> Maybe IsRelevant -> H.Html
annotationControl (AnnotationQueryId queryId) (ItemId item) groundTruthLabel =
    H.span ! H.dataAttribute "item"  item ! H.dataAttribute "query" queryId ! annotationClass $ mempty
  where
    annotationClass =  HA.class_ ("annotation " <> groundTruthHtml)
    groundTruthHtml =
      case groundTruthLabel of
        Just Relevant -> "poslabel"
        Just NotRelevant -> "neglabel"
        Nothing -> ""


 -- | A client-side query ID -- the query for which things are being annotated
newtype AnnotationQueryId = AnnotationQueryId H.AttributeValue

 -- | A client-side item ID -- the thing being annotated
newtype ItemId = ItemId H.AttributeValue

 -- | Binary relevance judgement
data IsRelevant = NotRelevant | Relevant
                deriving (Ord, Eq, Show)





-- ========= Tieing Annotation control together with paragraph rendering ===========


paragraphToItemId :: Paragraph -> ItemId
paragraphToItemId = ItemId . H.stringValue . unpackParagraphId . paraId

sectionPathToQueryId :: SectionPath -> AnnotationQueryId
sectionPathToQueryId = AnnotationQueryId . H.stringValue . escapeSectionPath

paragraphToAnnotationHtml' :: AnnotationQueryId -> Paragraph -> Maybe IsRelevant -> (Paragraph-> H.Html) -> H.Html
paragraphToAnnotationHtml' queryId p groundTruthLabel contentHtml =
    H.li ! HA.class_ "entity-snippet-li" $ do
        H.p $ do
            -- H.span ! HA.class_ "htmlscore" $ H.toHtml (show $ entryScore e)
            annotationControl queryId itemId groundTruthLabel
            H.span ! HA.class_ "entity-snippet-li-text" $ do      -- overview-snippets
                    contentHtml p
                    --paragraphToHtml p
  where
    itemId = paragraphToItemId p


paragraphToAnnotationHtml queryId p groundTruthLabel =  paragraphToAnnotationHtml' queryId p groundTruthLabel paragraphToHtml



-- ===== Renders Trec Car data types as (plain) Html ===========


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



-- =================== HTML prologue for annotationControl and Trec car data rendering   =================

prologue :: H.Html
prologue = do
    H.meta ! HA.charset "utf-8"
    H.script ! HA.src "http://code.jquery.com/jquery-1.11.0.min.js" $ mempty
    H.script ! HA.src "/annotate.js" $ mempty
    H.link ! HA.rel "stylesheet" ! HA.type_ "text/css" ! HA.href "http://smart-cactus.org/~dietz/knowledgeportfolios/css/bootstrap.min.css"
    H.link ! HA.rel "stylesheet" ! HA.type_ "text/css" ! HA.href "http://smart-cactus.org/~dietz/knowledgeportfolios/annotate.css"
    H.link ! HA.rel "stylesheet" ! HA.type_ "text/css" ! HA.href "http://smart-cactus.org/~dietz/knowledgeportfolios/qipidia.css"
