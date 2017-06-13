{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


-- | Construct passage views
module EntityViewHtml where

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

import TrecCarRenderHtml
import FileNameLookup

entityRankingToHtml :: SectionPathWithName -> [TrecCarRenderHtml.EntityRankingEntry] -> Maybe [TrecCarRenderHtml.EntityRankingEntry] -> H.Html
entityRankingToHtml spr@SectionPathWithName {..} sprRanking sprTruthsMaybe = H.docTypeHtml $ do
    H.head prologue
    H.body $ do
        viewHeader spr

        assessmentScaleInfo

        let renderHtml entry =
                entityToAnnotationHtml queryId (entryItem entry) Nothing   -- todo prio2 pass in relevance label instead of Nothing
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
                entityPassageToAnnotationHtml queryId (entryItem entry) Nothing
              where queryId = sectionPathToQueryId sprQueryId

        H.div ! HA.class_ "overview-wide" ! HA.class_ "overview-entities" $ do
            H.h1 "Entities with Passages"
            H.ol $ mapM_ renderHtml sprRanking  -- todo urgent sort by entity



