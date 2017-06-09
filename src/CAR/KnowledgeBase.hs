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
    , resolveRedirects
    , InlinkCounts(..)
    ) where

import Control.Exception (assert)
import Data.Monoid hiding (All, Any)

import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Maybe

import CAR.Utils
import CAR.Types

-- | actually: isPara
isLead :: PageSkeleton -> Bool
isLead (Para{}) = True
isLead (Section{}) = False
isLead (Image{}) = False

data KbDoc = KbDoc { kbDocPageId :: PageId
                   , kbDocCanonicalName :: PageName
                   , kbDocLeadText :: [T.Text]    -- ^ Page's lead paragraph text
                   , kbDocCategories :: [T.Text]  -- ^ Page's categories
                   , kbDocOutLinks :: [PageName]  -- ^ Targets of page's outgoing links
                   , kbDocOutMentions :: [T.Text] -- ^ Anchor text of page's outgoing links
                   , kbDocLeadPara :: [PageSkeleton]   -- ^ 'PageSkeleton's of lead paragraph text
                   }

data InlinkInfo = InlinkInfo { documentInlinks :: !(HM.HashMap PageName InlinkCounts)
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

---  sourcepage targetpage anchortext => attach anchortext to targetpage
---  redirect sourcepage targetpage   => attach sourcepage to targetpage
--   similar for disambiguation
resolveRedirects :: InlinkInfo -> HM.HashMap PageName InlinkCounts
resolveRedirects (InlinkInfo {..}) =
    fmap resolve documentInlinks
  where
    resolve :: InlinkCounts -> InlinkCounts
    resolve counts =
        counts <> mconcat [ assert (n==1) $ fromMaybe mempty $ HM.lookup rpage documentInlinks
                          | (rpage, n) <- HM.toList $ redirectCount counts ]


-- | Given a set of documents, build a map from target document to its 'InlinkCounts'
collectInlinkInfo :: [Page] -> InlinkInfo
collectInlinkInfo = foldMap pageInlinkInfo
  where
    one :: Hashable a => a -> HM.HashMap a Int
    one x = HM.singleton x 1

    pageInlinkInfo :: Page -> InlinkInfo
    pageInlinkInfo page
      | Just linkTarget <- pageRedirect page  =
            mempty { documentInlinks = HM.singleton linkTarget
                                       $ mempty { redirectCount = one $ pageName page }
                   , redirectPages = HS.singleton (pageName page)
                   }

      | pageIsDisambiguation page  =
            let toInlinkInfo link =
                    mempty { documentInlinks = HM.singleton (linkTarget link)
                                               $ mempty { disambiguationCount = one $ pageName page }
                           }
            in foldMap toInlinkInfo (pageLinks page)

      | otherwise  =
            let toInlinkInfo link =
                   mempty { documentInlinks = HM.singleton (linkTarget link)
                                              $ mempty { anchorCount = one $ linkAnchor link
                                                       , inLinkCounts = one $ linkTarget link }
                          }
            in foldMap toInlinkInfo (pageLinks page)

-- #(anchor, target) / #(anchor, *)
pageToKbDoc :: HM.HashMap PageName InlinkCounts -> Page -> KbDoc
pageToKbDoc inlinkInfoMap (Page pageName pageId pageSkeleta) =
  let leadParas = filter isLead $ pageSkeleta
      kbDocPageId = pageId
      kbDocLeadText = map TL.toStrict $ foldMap pageSkeletonText $ leadParas
      kbDocOutLinks = fmap linkTarget $ foldMap pageSkeletonLinks $ leadParas
      kbDocOutMentions = fmap linkAnchor $ foldMap pageSkeletonLinks $ leadParas
      kbDocLeadPara = leadParas
      kbDocCategories = pageCategories (Page pageName pageId pageSkeleta)
      kbDocCanonicalName = pageName

      inlinkInfo = fromMaybe mempty $ HM.lookup pageName inlinkInfoMap

      kbDocAnchorNames = anchorCount inlinkInfo -- todo consolidate multiple occurrences of the same name
      disambiguationNames = disambiguationCount inlinkInfo
      redirectNames = redirectCount inlinkInfo
  in KbDoc {..}
