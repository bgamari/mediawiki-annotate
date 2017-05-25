{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


-- | Construct passage views
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

import TrecCarRenderHtml
import FileNameLookup

passageRankingToHtml :: SectionPathWithName -> [TrecCarRenderHtml.PassageRankingEntry] -> Maybe [TrecCarRenderHtml.PassageRankingEntry] -> H.Html
passageRankingToHtml spr@SectionPathWithName {..} sprRanking sprTruthsMaybe = H.docTypeHtml $ do
    H.head prologue
    H.body $ do
        viewHeader spr

        H.p ! HA.class_ "entity-snippet-intro" $ "Assess relevance of passages for this section/article."
        H.p ! HA.class_ "entity-snippet-intro" $ "Assessment scale:"
        H.p ! HA.class_ "entity-snippet-intro" $ H.ol $ do
            H.li "Must: Must be mentioned"
            H.li "Should: Should be mentioned"
            H.li "Can: Can be mentioned"
            H.li "No: Not relevant for this section"
            H.li "Trash: Low-quality passage that is not useful for any section"
            H.li "Perfect: reserved for a passage that by itself says everything there needs to be said about this section"
            H.li "Eraser: delete assessment"

        let renderHtml entry =
                paragraphToAnnotationHtml queryId (entryItem entry) Nothing  -- todo prio2 : pass in relevance label instead of Nothing
              where queryId = sectionPathToQueryId sprQueryId

        H.div ! HA.class_ "overview-wide" ! HA.class_ "overview-entities" $ do
            H.h1 "Paragraphs"
            H.ol $ mapM_ renderHtml sprRanking


            case sprTruthsMaybe of
                Just sprTruths -> do
                    H.h1 "Automatic Ground Truth"
                    H.ol $ mapM_ renderHtml sprTruths
                Nothing -> mempty



