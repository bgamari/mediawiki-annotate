{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


-- | Construct passage views
module PassageViewHtml where

import Data.List
import Data.Function
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5.Attributes as HA
import Debug.Trace
import CAR.Types

import TrecCarRenderHtml

passageRankingToHtml :: SectionPathWithName -> [TrecCarRenderHtml.PassageRankingEntry] -> Maybe [TrecCarRenderHtml.PassageRankingEntry] -> H.Html
passageRankingToHtml spr@SectionPathWithName {..} sprRanking sprTruthsMaybe = H.docTypeHtml $ do
    H.head prologue
    H.body $ do
        viewHeader spr

        assessmentScaleInfo

        let renderHtml entry =
                paragraphToAnnotationHtml queryId (entryItem entry) []  -- todo prio2 : pass in relevance label instead of Nothing
              where queryId = sectionPathToQueryId sprQueryId

        H.div ! HA.class_ "overview-wide" ! HA.class_ "overview-entities" $ do
            H.h1 "Paragraphs"
            H.ol $ mapM_ renderHtml sprRanking


            case sprTruthsMaybe of
                Just sprTruths -> do
                    H.h1 "Automatic Ground Truth"
                    H.ol $ mapM_ renderHtml sprTruths
                Nothing -> mempty

passageMixedRankingToHtml :: SectionPathWithName -> [TrecCarRenderHtml.PassageRankingEntry] -> Maybe [TrecCarRenderHtml.PassageRankingEntry] -> H.Html
passageMixedRankingToHtml spr@SectionPathWithName {..} sprRanking sprTruthsMaybe = H.docTypeHtml $ do
    H.head prologue
    H.body $ do
        viewHeader spr

        assessmentScaleInfo

        let renderHtml entry =
                paragraphToAnnotationHtml queryId (entryItem entry) []  -- todo prio2 : pass in relevance label instead of Nothing
              where queryId = sectionPathToQueryId sprQueryId

        H.div ! HA.class_ "overview-wide" ! HA.class_ "overview-entities" $ do
            H.h1 "Paragraphs"


            case sprTruthsMaybe of
                Just sprTruths -> do
                    let mixed = nubBy ((==) `on` (paraId . entryItem)) $ sortBy (compare `on` (paraId . entryItem))
                               $ traceShow ("passageMixedRankingToHtml", (sprQueryId), sprRanking)
                               $ (sprRanking ++ sprTruths)
                    H.ol $ mapM_ renderHtml mixed
                Nothing -> mempty



