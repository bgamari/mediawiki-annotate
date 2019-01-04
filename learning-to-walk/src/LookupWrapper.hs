{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module LookupWrapper where

import Data.Maybe
import CAR.Types hiding (Entity)
import CAR.Utils
import CAR.TocFile as Toc
import qualified Data.HashMap.Strict as HM
import Data.Monoid hiding (All, Any)
import Data.Ord
import Control.DeepSeq
import GHC.Generics

import Codec.Serialise
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Data.Maybe

import qualified Data.SmallUtf8 as Utf8

-- ---------------------------------------------
-- Fetch pages from cbor
-- ---------------------------------------------

type PagesLookup =  ([PageId] -> [PageDoc])

readPagesToc :: Toc.IndexedCborPath PageId Page -> IO PagesLookup
readPagesToc pagesFileWithToc = do
    toc <- Toc.open pagesFileWithToc
    return $ \pageIds -> pagesToPageDocs $ mapMaybe ( `Toc.lookup` toc) pageIds

wrapPagesTocs :: HM.HashMap PageId PageDoc
                 -> PagesLookup
wrapPagesTocs pageId2Page =
    \pageIds -> catMaybes $ fmap (`HM.lookup` pageId2Page) pageIds





-- -----------------------------

data PageDoc = PageDoc { pageDocId              :: !PageId
                       , pageDocArticleId       :: !PageId
                       , pageDocNeighbors       :: !(HS.HashSet PageId)
                       , pageDocContent         :: !T.Text
                       }
           deriving (Show, Generic)


pageDocOnlyNeighbors :: PageDoc -> HS.HashSet PageId
pageDocOnlyNeighbors pageDoc = HS.filter (/= pageDocArticleId pageDoc) $ pageDocNeighbors pageDoc

instance Ord PageDoc where
    compare = comparing $ \x -> (pageDocId x, pageDocArticleId x)

instance Serialise PageDoc
instance NFData PageDoc

instance Eq PageDoc where
    x == y =
           pageDocId x == pageDocId y
        && pageDocArticleId x == pageDocArticleId y

instance Hashable PageDoc where
    hashWithSalt salt x =
        hashWithSalt salt (pageDocId x, pageDocArticleId x)




-- -------  build ------------------

pageToPageDocs :: Page -> [PageDoc]
pageToPageDocs page =
    [convertPage page]
  where
    convertPage :: Page -> PageDoc
    convertPage page@(Page pageName pageId _ _ _)  =
      let
        pageDocId             = pageId
        pageDocArticleId      = pageId
        pageDocNeighbors      = HS.fromList
                              $ [pageId] ++ pageLinkTargetIds page
        pageDocContent        = ""
      in PageDoc {..}


pageDocHasLinks :: PageDoc -> Bool
pageDocHasLinks p =  (>1) $ length $ pageDocNeighbors p

pagesToPageDocs :: [Page] -> [PageDoc]
pagesToPageDocs =
    foldMap (pageToPageDocs)

