{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


-- | Construct passage views
module EntityViewHtml where

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5.Attributes as HA
import CAR.Types
import Data.Function
import Data.List
import Debug.Trace

import TrecCarRenderHtml

entityRankingToHtml :: SectionPathWithName -> [TrecCarRenderHtml.EntityRankingEntry] -> Maybe [TrecCarRenderHtml.EntityRankingEntry] -> H.Html
entityRankingToHtml spr@SectionPathWithName {..} sprRanking sprTruthsMaybe = H.docTypeHtml $ do
    H.head prologue
    H.body $ do
        viewHeader spr

        assessmentScaleInfo

        let renderHtml entry =
                entityToAnnotationHtml queryId (entryItem entry) []   -- todo prio2 pass in relevance label instead of Nothing
              where queryId = sectionPathToQueryId sprQueryId

        H.div ! HA.class_ "overview-wide" ! HA.class_ "overview-entities" $ do
            H.h1 "Entities"
            H.ol $ mapM_ renderHtml sprRanking


            case sprTruthsMaybe of
                Just sprTruths -> do
                    H.h1 "Automatic Ground Truth"
                    H.ol $ mapM_ renderHtml sprTruths
                Nothing -> mempty

entityPassageRankingToHtml :: SectionPathWithName -> [TrecCarRenderHtml.EntityParagraphRankingEntry] -> Maybe [TrecCarRenderHtml.EntityParagraphRankingEntry] -> H.Html
entityPassageRankingToHtml spr@SectionPathWithName {..} sprRanking sprTruthsMaybe = H.docTypeHtml $ do
    H.head prologue
    H.body $ do
        viewHeader spr

        assessmentScaleInfo
        let renderHtml entry =
                entityPassageToAnnotationHtml queryId (entryItem entry) []
              where queryId = sectionPathToQueryId sprQueryId

            toId :: (Entity, Paragraph)  -> (PageId, ParagraphId)
            toId (entity, paragraph) = (entityPageId entity, paraId paragraph)

        H.div ! HA.class_ "overview-wide" ! HA.class_ "overview-entities" $ do
            H.h1 "Entities with Passages"
            --H.ol $ mapM_ renderHtml sprRanking  -- todo urgent sort by entity


            case sprTruthsMaybe of
                Just sprTruths -> do
                    let mixed = nubBy ((==) `on` (toId . entryItem)) $ sortBy (compare `on` (toId . entryItem))
                               $ traceShow ("entityViewHtml", (sprQueryId), sprRanking)
                               $ (sprRanking ++ sprTruths)
                    H.ol $ mapM_ renderHtml mixed
                Nothing -> do
                    let mixed = nubBy ((==) `on` (toId . entryItem)) $ sortBy (compare `on` (toId . entryItem))
                               $ traceShow ("entityViewHtml", (sprQueryId), sprRanking)
                               $ (sprRanking)
                    H.ol $ mapM_ renderHtml mixed




