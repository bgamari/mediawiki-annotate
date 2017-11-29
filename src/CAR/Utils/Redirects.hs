{-# LANGUAGE BangPatterns #-}

module CAR.Utils.Redirects where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Maybe
import Control.Monad

import CAR.Types
import CAR.Utils
import SimplIR.Utils.Compact

resolveRedirects :: [Page]
                 -> (PageId -> PageId)
resolveRedirects pages =
    resolveRedirectFun entityRedirects
  where
    !entityRedirects = inCompact $ entityRedirectMap pages

resolveRedirectFun :: HM.HashMap PageId PageId -> PageId -> PageId
resolveRedirectFun entityRedirects origFromPageId = go mempty origFromPageId
  where
    go :: HS.HashSet PageId -> PageId -> PageId
    go history fromPageId
      | fromPageId `HS.member` history  = origFromPageId --  we are walking in circles, return original.
      | Just toPageId <- HM.lookup fromPageId entityRedirects = go (fromPageId `HS.insert` history)  toPageId  -- follow redirect
      | otherwise = fromPageId  -- success, we found a real page

entityRedirectMap :: [Page] -> HM.HashMap PageId PageId
entityRedirectMap pages =
    HM.fromList $ mapMaybe extractRedirect pages
  where extractRedirect :: Page -> Maybe (PageId, PageId)
        extractRedirect page@(Page {pageId=fromPageId})
          | isNullPageId fromPageId = Nothing
          | otherwise = do
            toPageId <- pageRedirect page  -- MaybeMonad
            guard $ not $ isNullPageId toPageId      -- if empty string -> Nothing
            pure (fromPageId, toPageId)

        isNullPageId :: PageId -> Bool
        isNullPageId = null . unpackPageId
