{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

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


data WhichNeighbors = NeighborsFromOutlinks | NeighborsFromInlinks
    deriving (Show, Read, Ord, Eq, Enum, Bounded, Generic, Serialise, Hashable)

-- ---------------------------------------------
-- Fetch pages from cbor
-- ---------------------------------------------

type AbstractLookup id =  ([id] -> [AbstractDoc id])

type PagesLookup = AbstractLookup PageId


readPagesToc :: Toc.IndexedCborPath PageId Page -> IO ([PageId] -> [Page])
readPagesToc pagesFileWithToc = do
    toc <- Toc.open pagesFileWithToc
    return $ \pageIds -> mapMaybe ( `Toc.lookup` toc) pageIds

readAbstractDocToc :: (Hashable id, Eq id, Serialise id, NFData id)
                   => Toc.IndexedCborPath id (AbstractDoc id) -> IO (AbstractLookup id)
readAbstractDocToc pagesFileWithToc = do
    toc <- Toc.open pagesFileWithToc
    return $ \pageIds ->  mapMaybe ( `Toc.lookup` toc) pageIds

wrapPagesTocs :: (Hashable id, Eq id)
                 => HM.HashMap id (AbstractDoc id)
                 -> AbstractLookup id
wrapPagesTocs pageId2Page =
    \pageIds -> catMaybes $ fmap (`HM.lookup` pageId2Page) pageIds





-- -----------------------------

data AbstractDoc pageDocId = AbstractDoc { pageDocId              :: !pageDocId
                                         , pageDocArticleId       :: !PageId
                                         , pageDocNeighbors       :: !(HS.HashSet PageId)
                                         , pageDocContent         :: !T.Text
                                         }
                       deriving (Show, Generic)

type PageDoc = AbstractDoc PageId


pageDocOnlyNeighbors :: PageDoc -> HS.HashSet PageId
pageDocOnlyNeighbors pageDoc = HS.filter (/= pageDocArticleId pageDoc) $ pageDocNeighbors pageDoc

instance Ord id => Ord (AbstractDoc id) where
    compare = comparing $ \x -> (pageDocId x, pageDocArticleId x)

instance Serialise id => Serialise (AbstractDoc id)
instance NFData id => NFData (AbstractDoc id)

instance Eq id => Eq (AbstractDoc id) where
    x == y =
           pageDocId x == pageDocId y
        && pageDocArticleId x == pageDocArticleId y

instance Hashable id => Hashable (AbstractDoc id) where
    hashWithSalt salt x =
        hashWithSalt salt (pageDocId x, pageDocArticleId x)




-- -------  build ------------------

pageToPageDocs :: [WhichNeighbors] -> Page -> [PageDoc]
pageToPageDocs whichNeighbors page =
    [convertPage page]
  where
    fetchNeighbors :: Page -> WhichNeighbors -> [PageId]
    fetchNeighbors page whichNeighbor =
        case whichNeighbor of
            NeighborsFromOutlinks -> pageLinkTargetIds page
            NeighborsFromInlinks -> fromMaybe [] $ getMetadata _InlinkIds (pageMetadata page)
            _ -> []

    convertPage :: Page -> PageDoc
    convertPage page@(Page pageName pageId _ _ _)  =
      let
        pageDocId             = pageId
        pageDocArticleId      = pageId
        pageDocNeighbors      = HS.fromList
                              $ [pageId] ++ concat (fmap (fetchNeighbors page) whichNeighbors)
        pageDocContent        = ""
      in AbstractDoc {..}


pageDocHasLinks :: AbstractDoc id -> Bool
pageDocHasLinks p =  (>1) $ length $ pageDocNeighbors p

pagesToPageDocs :: [WhichNeighbors] -> [Page] -> [PageDoc]
pagesToPageDocs whichNeighbors =
    foldMap (pageToPageDocs whichNeighbors)

