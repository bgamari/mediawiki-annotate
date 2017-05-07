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

passageRankingToHtml :: SectionPathWithName -> [TrecCarRenderHtml.RankingEntry] -> H.Html
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
                renderHtml TrecCarRenderHtml.RankingEntry {..} =
                    paragraphToAnnotationHtml queryId entryParagraph Nothing
                  where queryId = sectionPathToQueryId sprQueryId
