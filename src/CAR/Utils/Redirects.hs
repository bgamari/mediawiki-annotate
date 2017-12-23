{-# LANGUAGE BangPatterns #-}

module CAR.Utils.Redirects where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Maybe
import Control.Monad
import qualified Data.Text as T

import CAR.Types
import CAR.Utils
import SimplIR.Utils.Compact

-- resolveRedirects :: [Page]
--                  -> (PageId -> PageId)
-- resolveRedirects pages =
--     fst . resolveRedirectFun entityRedirects
--   where
--     !entityRedirects = inCompact $ entityRedirectMap pages

resolveRedirectsWithSection :: [Page]
                 -> (PageId -> (PageId, Maybe T.Text))
resolveRedirectsWithSection pages =
    resolveRedirectFun entityRedirects
  where
    !entityRedirects = inCompact $ entityRedirectMap pages


resolveRedirectFun :: HM.HashMap PageId Link -> PageId -> (PageId, Maybe T.Text)
resolveRedirectFun entityRedirects origFromPageId = go mempty (origFromPageId, Nothing)
  where
    go :: HS.HashSet PageId -> (PageId, Maybe T.Text) -> (PageId, Maybe T.Text)
    go history (fromPageId, fromMaybeSection)
      | fromPageId `HS.member` history  = (origFromPageId, Nothing) --  we are walking in circles, return original.
      | Just Link{ linkTargetId = toPageId, linkSection = Nothing} <- HM.lookup fromPageId entityRedirects
      = go (fromPageId `HS.insert` history)  (toPageId, fromMaybeSection)  -- follow redirect without section
      | Just Link{ linkTargetId = toPageId, linkSection = Just toSection} <- HM.lookup fromPageId entityRedirects
      = go (fromPageId `HS.insert` history)  (toPageId, Just toSection)  -- follow redirect with section
      | otherwise = (fromPageId, fromMaybeSection)  -- success, we found a real page when no redirect found

--
-- resolveRedirectFun :: HM.HashMap PageId Link -> PageId -> PageId
-- resolveRedirectFun entityRedirects origFromPageId = go mempty origFromPageId
--   where
--     go :: HS.HashSet PageId -> PageId -> PageId
--     go history fromPageId
--       | fromPageId `HS.member` history  = origFromPageId --  we are walking in circles, return original.
--       | Just Link{ linkTargetId = toPageId} <- HM.lookup fromPageId entityRedirects =
--             go (fromPageId `HS.insert` history)  toPageId  -- follow redirect
--       | otherwise = fromPageId  -- success, we found a real page


entityRedirectMap :: [Page] -> HM.HashMap PageId Link
entityRedirectMap pages =
    HM.fromList $ mapMaybe extractRedirect pages
  where extractRedirect :: Page -> Maybe (PageId, Link)
        extractRedirect page@(Page {pageId=fromPageId})
          | isNullPageId fromPageId = Nothing
          | otherwise = do
            redirectLink <- pageRedirect page  -- MaybeMonad
            guard $ not $ isNullPageId  (linkTargetId  redirectLink)      -- if empty string -> Nothing
            pure (fromPageId, redirectLink)

        isNullPageId :: PageId -> Bool
        isNullPageId = null . unpackPageId
