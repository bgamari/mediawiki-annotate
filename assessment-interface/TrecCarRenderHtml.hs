{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


-- | Types and reusable HTML rendering
module TrecCarRenderHtml where

import Control.Monad
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!), p, ul, li, toHtml, div)
import Text.Blaze.Html5.Attributes as HA
import qualified Text.Blaze.Html.Renderer.Utf8 as H
import qualified Data.ByteString.Lazy as BSL

import CAR.Utils
import CAR.Types

import qualified SimplIR.Format.TrecRunFile as TrecRun
import qualified SimplIR.Format.QRel as TrecQrel


data RankingEntry item = RankingEntry { entryItem  :: item
                                      , entryScore :: TrecRun.Score
                                      }
                       | QrelEntry { entryItem  :: item
                                   , entryLabel :: IsRelevant}
     deriving Show


type EntityRankingEntry = RankingEntry Entity
type EntityParagraphRankingEntry = RankingEntry (Entity, Paragraph)
type PassageRankingEntry = RankingEntry Paragraph



data SectionPathWithName = SectionPathWithName { sprQueryId      :: SectionPath
                                               , sprPageName     :: PageName
                                               , sprHeadingPath  :: [SectionHeading]
                                               }
      deriving Show


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

fromBinaryRelevance :: TrecQrel.IsRelevant -> IsRelevant
fromBinaryRelevance simplirRelevance =
    case simplirRelevance of
      TrecQrel.Relevant -> Relevant
      TrecQrel.NotRelevant -> NotRelevant




-- ========= Tieing Annotation control together with paragraph rendering ===========


paragraphToItemId :: Paragraph -> ItemId
paragraphToItemId = ItemId . H.stringValue . unpackParagraphId . paraId

sectionPathToQueryId :: SectionPath -> AnnotationQueryId
sectionPathToQueryId = AnnotationQueryId . H.stringValue . escapeSectionPath

paragraphToAnnotationHtml' :: AnnotationQueryId -> Paragraph -> Maybe IsRelevant -> (Paragraph-> H.Html) -> H.Html
paragraphToAnnotationHtml' queryId para groundTruthLabel contentHtml =
    H.li ! HA.class_ "entity-snippet-li" $ do
        H.p $ do
            -- H.span ! HA.class_ "htmlscore" $ H.toHtml (show $ entryScore e)
            annotationControl queryId itemId groundTruthLabel   -- todo prio2 show ground truth label in annotation
            H.span ! HA.class_ "entity-snippet-li-text" $ do
                    contentHtml para
  where
    itemId = paragraphToItemId para


paragraphToAnnotationHtml queryId p groundTruthLabel =  paragraphToAnnotationHtml' queryId p groundTruthLabel paragraphToHtml


-- ============= Tieing Annotation Control together with entity rendering ===========

entityToItemId :: Entity -> ItemId
entityToItemId = ItemId . H.stringValue . unpackPageId . entityPageId



entityToAnnotationHtml' :: AnnotationQueryId -> Entity -> Maybe IsRelevant -> (Entity-> H.Html) -> H.Html
entityToAnnotationHtml' queryId entity groundTruthLabel contentHtml =
    H.li ! HA.class_ "entity-snippet-li" $ do
        H.p $ do
            -- H.span ! HA.class_ "htmlscore" $ H.toHtml (show $ entryScore e)
            annotationControl queryId itemId groundTruthLabel  -- todo prio2 show ground truth label in annotation
            H.span ! HA.class_ "entity-snippet-li-text" $ do
                    contentHtml entity

  where
    itemId = entityToItemId entity


entityToAnnotationHtml queryId entity groundTruthLabel =
    entityToAnnotationHtml' queryId entity groundTruthLabel entityToHtml


-- ============= Tieing Annotation Control together with entity-passage rendering ===========

entityPassageToItemId :: (Entity, Paragraph) -> ItemId
entityPassageToItemId (entity, para) =
    let e = unpackPageId . entityPageId $ entity
        p = unpackParagraphId . paraId $ para
    in ItemId $ H.stringValue $  p <> "/" <> e


entityPassageToAnnotationHtml' :: AnnotationQueryId -> (Entity, Paragraph) -> Maybe IsRelevant -> (Entity-> H.Html) -> (Paragraph -> H.Html) -> H.Html
entityPassageToAnnotationHtml' queryId (entity, paragraph) groundTruthLabel contentEntityHtml contentParagraphHtml =
    H.li ! HA.class_ "entity-snippet-li" $ do
        H.p $ do
            -- H.span ! HA.class_ "htmlscore" $ H.toHtml (show $ entryScore e)
            annotationControl queryId itemId groundTruthLabel  -- todo prio2 show ground truth label in annotation
            H.span ! HA.class_ "entity-snippet-li-text" $ do
                    H.div ! HA.class_ "passage-orig-entity" $ contentEntityHtml entity
                    H.p $ contentParagraphHtml paragraph

  where
    itemId = entityPassageToItemId (entity, paragraph)


entityPassageToAnnotationHtml :: AnnotationQueryId -> (Entity, Paragraph) -> Maybe IsRelevant -> H.Html
entityPassageToAnnotationHtml queryId (entity, paragraph) groundTruthLabel =
    entityPassageToAnnotationHtml' queryId (entity,paragraph) groundTruthLabel entityToHtml paragraphToHtml


-- === Pretty section Path ====
viewHeader :: SectionPathWithName -> H.Html
viewHeader spr@SectionPathWithName{..} = do
      H.div ! HA.class_ "overview-heading-query" $ do
        prettySectionPath spr

        H.p $ wikipediaSection spr "Wikipedia"

        H.div $ do
            H.span "Query ID: "
            H.code $ toHtml (escapeSectionPath sprQueryId)

      H.div $ do
        H.p $ do
              "Back to "
              H.a ! HA.href "./" $ "Outline"
              " / "
              H.a ! HA.href "../" $ "Topic List"


prettySectionPath :: SectionPathWithName -> H.Html
prettySectionPath SectionPathWithName{..} =
    toNestedList (toHtml (getPageName sprPageName) : map (toHtml . getSectionHeading) sprHeadingPath)

toNestedList :: [H.Html] -> H.Html
toNestedList [] = mempty
toNestedList (x:xs) =
    H.ul $ H.li $ x <> toNestedList xs





-- ===== Renders Trec Car data types as (plain) Html ===========


entityToHtml :: Entity -> H.Html
entityToHtml Entity{..} =
    let url = H.textValue $ "https://wikipedia.org/wiki/"<> (getPageName entityPageName)  -- todo prio2 pageName or pageId? (url encoding issues)
    in H.a ! HA.href url $ toHtml (getPageName entityPageName)



paragraphToHtml :: Paragraph -> H.Html
paragraphToHtml p = foldMap paraBodyToHtml (paraBody p)

-- | Given a string, drop all text between the given beginning and end substrings.
dropSpans :: T.Text -> T.Text -> T.Text -> TL.Text
dropSpans begin end = go
  where
    go t
      | T.null match = TL.fromStrict prefix
      | otherwise    =
        case T.breakOn end match of
          (tagBody, endMatch)
            | T.null endMatch -> TL.fromStrict t
            | otherwise       -> let suffix = T.drop (T.length end) endMatch
                                 in TL.fromStrict prefix <> go suffix
      where (prefix, match) = T.breakOn begin t
      
dropRefs :: T.Text -> TL.Text
dropRefs = dropSpans "<ref " "/>"

paraBodyToHtml :: ParaBody -> H.Html
paraBodyToHtml (ParaText t) = H.text $ TL.toStrict $ dropRefs t
paraBodyToHtml (ParaLink l) = wikipediaLink l $ H.toHtml $ linkAnchor l

-- | A link to a Wikipedia page
wikipediaPage' :: PageName -> H.Html -> H.Html
wikipediaPage' pageName =
    H.a ! HA.href (H.textValue $ "https://en.wikipedia.org/wiki/" <> getPageName pageName)

-- | A link to a Wikipedia page, using the page name as the anchor text
wikipediaPage :: PageName -> H.Html
wikipediaPage page = wikipediaPage' page (H.toHtml $ getPageName page)
    
wikipediaLink :: Link -> H.Html -> H.Html
wikipediaLink l body =
    H.a ! HA.href url $ body
  where
    PageName n = linkTarget l
    url = H.textValue $ "https://wikipedia.org/wiki/"<>n<>section'
    section' = case linkSection l of
                 Just sect -> "#"<>sect
                 Nothing   -> ""


wikipediaSection :: SectionPathWithName -> H.Html -> H.Html
wikipediaSection spwn@SectionPathWithName{..} body =
    H.a ! HA.href url $ body
  where
    PageName n = sprPageName
    url = H.textValue $ "https://wikipedia.org/wiki/"<>n<>section'
    section' = case sprHeadingPath of
                 []  -> ""
                 headings -> "#"<> T.map (\c -> if c==' ' then '_' else c) (getSectionHeading $ last headings)


-- =================== HTML prologue for annotationControl and Trec car data rendering   =================

prologue :: H.Html
prologue = do
    H.meta ! HA.charset "utf-8"
    H.script ! HA.src "http://code.jquery.com/jquery-1.11.0.min.js" $ mempty
    H.script ! HA.src "/annotate.js" $ mempty
    H.link ! HA.rel "stylesheet" ! HA.type_ "text/css" ! HA.href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
    H.link ! HA.rel "stylesheet" ! HA.type_ "text/css" ! HA.href "/annotate.css"
    H.link ! HA.rel "stylesheet" ! HA.type_ "text/css" ! HA.href "/assessment-interface-v1.css"


assessmentScaleInfo :: H.Html
assessmentScaleInfo = do
        H.p ! HA.class_ "entity-snippet-intro" $ "Assess relevance of passages for this section/article."
        H.p ! HA.class_ "entity-snippet-intro" $ "Assessment scale (Use Must/Can/No whenever possible):"
        H.p ! HA.class_ "entity-snippet-intro" $ H.ol $ do
            H.li "Perfect: reserved for a passage that by itself says everything there needs to be said about this section"
            H.li $ H.b "Must: Must be mentioned"
            H.li "Should: Should be mentioned"
            H.li $ H.b "Can: Can be mentioned"
            H.li $ H.b "No: Not relevant for this section"
            H.li "Trash: Low-quality passage that is not useful for any section"
            H.li "Eraser: delete assessment"
