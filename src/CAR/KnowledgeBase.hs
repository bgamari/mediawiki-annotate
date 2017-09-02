{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module CAR.KnowledgeBase
    ( -- * Knowledge base documents
      KbDoc(..)
    , pageToKbDoc

      -- * Inlink statistics
    , InlinkInfo(..)
    , collectInlinkInfo
--     , resolveRedirects
    , InlinkCounts(..)
    ) where

import Data.Monoid hiding (All, Any)

import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import CAR.Utils
import CAR.Types

-- | actually: isPara
isLead :: PageSkeleton -> Bool
isLead (Para{}) = True
isLead (Section{}) = False
isLead (Image{}) = False
isLead (List{}) = False

data KbDoc = KbDoc { kbDocPageId :: PageId
                   , kbDocCanonicalName :: PageName
                   , kbDocLeadText :: [T.Text]    -- ^ Page's lead paragraph text
                   , kbDocCategories :: [T.Text]  -- ^ Page's categories
                   , kbDocOutLinks :: [PageName]  -- ^ Targets of page's outgoing links
                   , kbDocOutMentions :: [T.Text] -- ^ Anchor text of page's outgoing links
                   , kbDocLeadPara :: [PageSkeleton]   -- ^ 'PageSkeleton's of lead paragraph text
                   , kbDocFullText :: [TL.Text]   -- ^ 'Page's full text including title
                   }

data InlinkInfo = InlinkInfo { documentInlinks :: !(HM.HashMap PageId InlinkCounts)
                             , redirectPages   :: !(HS.HashSet PageName)
                             }
                deriving (Show)

instance Monoid InlinkInfo where
    mempty = InlinkInfo mempty mempty
    InlinkInfo a b `mappend` InlinkInfo a' b' =
        InlinkInfo (HM.unionWith mappend a a') (b<>b')

data InlinkCounts = InlinkCounts { -- | How many time each anchor is used to link to this document.
                                   inLinkCounts        :: !(HM.HashMap PageName Int)
                                 , anchorCount         :: !(HM.HashMap T.Text Int)
                                 , disambiguationCount :: !(HM.HashMap PageName Int)
                                 , redirectCount       :: !(HM.HashMap PageName Int)
                                 }
                  deriving (Show)

instance Monoid InlinkCounts where
    mempty = InlinkCounts mempty mempty mempty mempty
    InlinkCounts a b c d `mappend` InlinkCounts a' b' c' d' =
        InlinkCounts (HM.unionWith (+) a a')
                     (HM.unionWith (+) b b')
                     (HM.unionWith (+) c c')
                     (HM.unionWith (+) d d')

--  sourcepage targetpage anchortext => attach anchortext to targetpage
--  redirect sourcepage targetpage   => attach sourcepage to targetpage
--   similar for disambiguation
-- resolveRedirects :: InlinkInfo -> HM.HashMap PageName InlinkCounts
-- resolveRedirects (InlinkInfo {..}) =
--     fmap resolve documentInlinks
--   where
--     resolve :: InlinkCounts -> InlinkCounts
--     resolve inlinkCounts =
--         inlinkCounts <> mconcat [ assert (n==1) $ fromMaybe mempty $ HM.lookup rpage documentInlinks
--                           | (rpage, n) <- HM.toList $ redirectCount inlinkCounts ]


-- | Given a set of documents, build a map from target document to its 'InlinkCounts'
collectInlinkInfo :: SiteId -> (PageId -> PageId) -> [Page] -> InlinkInfo
collectInlinkInfo siteId resolveRedirects' = foldMap pageInlinkInfo
  where
    one :: Hashable a => a -> HM.HashMap a Int
    one x = HM.singleton x 1

    pageInlinkInfo :: Page -> InlinkInfo
    pageInlinkInfo page
      | Just redirTargetId <- pageRedirect page  =
            mempty { documentInlinks = HM.singleton (resolveRedirects' $ pageNameToId siteId redirTargetId)
                                       $ mempty { redirectCount = one $ pageName page }
                   , redirectPages = HS.singleton (pageName page)
                   }

      | pageIsDisambiguation page  =
            let toInlinkInfo link =
                    mempty { documentInlinks = HM.singleton (resolveRedirects' $ linkTargetId link)
                                               $ mempty { disambiguationCount = one $ pageName page }
                           }
            in foldMap toInlinkInfo (pageLinks page)

      | otherwise  =
            let toInlinkInfo link =
                   mempty { documentInlinks =
                               HM.singleton
                                   (resolveRedirects' (linkTargetId link))
                                   (mempty { anchorCount = one $ linkAnchor link
                                           , inLinkCounts = one $ linkTarget link })
                          }
            in foldMap toInlinkInfo (pageLinks page)    -- Map [toPage, InLinkInfo (fromPage's outlinks)]

-- #(anchor, target) / #(anchor, *)
pageToKbDoc :: Page -> KbDoc
pageToKbDoc page =
  let leadParas = filter isLead $ pageSkeleton page
      kbDocPageId = pageId page
      kbDocLeadText = map TL.toStrict $ foldMap pageSkeletonFulltext $ leadParas
      kbDocOutLinks = fmap linkTarget $ foldMap pageSkeletonLinks $ leadParas
      kbDocOutMentions = fmap linkAnchor $ foldMap pageSkeletonLinks $ leadParas
      kbDocLeadPara = leadParas
      -- FIXME
      --kbDocCategories = pageCategories $ pageMetadata page
      kbDocCanonicalName = pageName page
      kbDocFullText = pageFulltext page
  in KbDoc {..}
