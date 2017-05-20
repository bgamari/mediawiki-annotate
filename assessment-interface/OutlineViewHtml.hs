{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


-- | Construct passage views
module OutlineViewHtml where

import Control.Monad
import Data.Monoid
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!), toHtml)
import Text.Blaze.Html5.Attributes as HA
import qualified Text.Blaze.Html.Renderer.Utf8 as H
import qualified Data.ByteString.Lazy as BSL
import Network.URI

import CAR.Utils
import CAR.Types
import CAR.CarExports
import qualified SimplIR.Format.TrecRunFile as Run

import TrecCarRenderHtml
import FileNameLookup

-- todo generate links to both passageView and entityView  (separate page for ground truth?)

outlineToHtml :: FileNameLookup -> Stub -> H.Html
outlineToHtml FileNameLookup{..} outline@(Stub pageName pageId skeleta) = H.docTypeHtml $ do
    H.head prologue
    H.body $ do
        H.h1 $ "Title "
            <> toHtml (getPageName pageName)
        H.p $ " (" <> toHtml (unpackPageId pageId) <> ") "

        H.div $ do
            H.p $ do
                  "Back to "
                  H.a ! HA.href ".." $ "Query List"

        H.div $ do
            H.ol $ mapM_ (H.li . (renderHtml [])) skeleta

  where
    renderHtml :: [HeadingId] -> PageSkeleton -> H.Html
    renderHtml headingPath (Para _) = mempty
    renderHtml headingPath (Image _ _) = mempty
    renderHtml headingPath (Section sectionHeading headingId children)  =  do
        let headingPath' = (headingPath ++ [headingId])
        let sectionPath = SectionPath pageId headingPath'
        let maybeFileURL = maybePassageViewUrl sectionPath

        H.span ! HA.class_ "heading"  $ do
            wrapHyperlink maybeFileURL $ H.toHtml $ getSectionHeading sectionHeading
            H.ol $ mapM_ (H.li . (renderHtml headingPath' ) ) children

        where wrapHyperlink :: (Maybe FilePath) -> H.Html -> H.Html
              wrapHyperlink maybeFileURL headingHtml =
                  case maybeFileURL of
                    Just fileURL -> H.a ! HA.href (H.stringValue $ "../" <> fileURL) $ headingHtml
                    Nothing -> headingHtml
