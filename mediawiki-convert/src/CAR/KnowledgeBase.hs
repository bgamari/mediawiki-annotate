{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module CAR.KnowledgeBase
    ( -- * Knowledge base documents
      KbDoc(..)
    , pageToKbDoc

      -- * Inlink statistics
    , InlinkInfo(..)
    -- TODO Obsolete, collectInlinkInfo
    , InlinkCounts(..)
    ) where

import Data.Semigroup

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
isLead (Infobox{}) = False

data KbDoc = KbDoc { kbDocPageId :: PageId
                   , kbDocCanonicalName :: PageName
                   , kbDocLeadText :: [T.Text]    -- ^ Page's lead paragraph text
                   , kbDocCategories :: [T.Text]  -- ^ Page's categories
                   , kbDocOutLinks :: [PageName]  -- ^ Targets of page's outgoing links
                   , kbDocOutMentions :: [T.Text] -- ^ Anchor text of page's outgoing links
                   , kbDocLeadPara :: [PageSkeleton]   -- ^ 'PageSkeleton's of lead paragraph text
                   , kbDocFullText :: [TL.Text]   -- ^ 'Page's full text including title
                   , kbDocMetadata :: PageMetadata
                   }

data InlinkInfo = InlinkInfo { documentInlinks :: !(HM.HashMap PageId InlinkCounts)
                             , redirectPages   :: !(HS.HashSet PageName)
                             }
                deriving (Show)

instance Semigroup InlinkInfo where
    InlinkInfo a b <> InlinkInfo a' b' =
        InlinkInfo (HM.unionWith (<>) a a') (b<>b')

instance Monoid InlinkInfo where
    mempty = InlinkInfo mempty mempty
    mappend = (<>)

data InlinkCounts = InlinkCounts { -- | How many time each anchor is used to link to this document.
                                   inLinkCounts        :: !(HM.HashMap PageName Int)
                                 , anchorCount         :: !(HM.HashMap T.Text Int)
                                 , disambiguationCount :: !(HM.HashMap PageName Int)
                                 , redirectCount       :: !(HM.HashMap PageName Int)
                                 }
                  deriving (Show)

instance Semigroup InlinkCounts where
    InlinkCounts a b c d <> InlinkCounts a' b' c' d' =
        InlinkCounts (HM.unionWith (+) a a')
                     (HM.unionWith (+) b b')
                     (HM.unionWith (+) c c')
                     (HM.unionWith (+) d d')

instance Monoid InlinkCounts where
    mempty = InlinkCounts mempty mempty mempty mempty
    mappend = (<>)

-- TODO: Obsolete: Use the information from page meta data
--
-- -- | Given a set of documents, build a map from target document to its 'InlinkCounts'
-- collectInlinkInfo :: [Page] -> InlinkInfo
-- collectInlinkInfo = foldMap pageInlinkInfo
--   where
--     one :: Hashable a => a -> HM.HashMap a Int
--     one x = HM.singleton x 1
--
--     pageInlinkInfo :: Page -> InlinkInfo
--     pageInlinkInfo page
--       | Just redirTargetId <- pageRedirect page  =
--             mempty { documentInlinks = HM.singleton redirTargetId
--                                        $ mempty { redirectCount = one $ pageName page }
--                    , redirectPages = HS.singleton (pageName page)
--                    }
--
--       | pageIsDisambiguation page  =
--             let toInlinkInfo link =
--                     mempty { documentInlinks = HM.singleton (linkTargetId link)
--                                                $ mempty { disambiguationCount = one $ pageName page }
--                            }
--             in foldMap toInlinkInfo (pageLinks page)
--
--       | otherwise  =
--             let toInlinkInfo link =
--                    mempty { documentInlinks =
--                                HM.singleton
--                                    (linkTargetId link)
--                                    (mempty { anchorCount = one $ linkAnchor link
--                                            , inLinkCounts = one $ linkTarget link })
--                           }
--             in foldMap toInlinkInfo (pageLinks page)    -- Map [toPage, InLinkInfo (fromPage's outlinks)]

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
      kbDocMetadata = pageMetadata page
  in KbDoc {..}
