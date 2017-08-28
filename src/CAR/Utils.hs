{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module CAR.Utils where

import Control.Monad (guard)
import Data.Maybe
import qualified Data.DList as DList
import           Data.DList (DList)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import CAR.Types
import SimplIR.Utils.Compact

pageRedirect :: Page -> Maybe PageName
pageRedirect (Page {pageSkeleton=Para (Paragraph _ (ParaText t : rest)) : _})
  | T.pack "#redirect" `T.isPrefixOf` T.toCaseFold (T.stripStart t)
  , (ParaLink l) : _ <- rest = Just (linkTarget l)
pageRedirect _ = Nothing

pageIsDisambiguation :: Page -> Bool
pageIsDisambiguation (Page { pageName = PageName t }) =
    (T.pack " (disambiguation)") `T.isInfixOf` T.toCaseFold t

pageContainsText :: T.Text -> Page -> Bool
pageContainsText str = any goSkeleton . pageSkeleton
  where
    goSkeleton (Section _ _ children) = any goSkeleton children
    goSkeleton (Para (Paragraph _ bodies)) = any goParaBody bodies
    goSkeleton (Image {}) = False

    goParaBody (ParaLink l) = str `T.isInfixOf` linkAnchor l
    goParaBody (ParaText t) = str `T.isInfixOf` t

pageLinkTargets :: Page -> [PageName]
pageLinkTargets = map linkTarget . pageLinks

pageLinks :: Page -> [Link]
pageLinks = foldMap pageSkeletonLinks . pageSkeleton

pageParas :: Page -> [Paragraph]
pageParas = foldMap pageSkeletonParas . pageSkeleton


pageSkeletonParas :: PageSkeleton -> [Paragraph]
pageSkeletonParas (Section _ _ children) = foldMap pageSkeletonParas children
pageSkeletonParas (Para (paragraph)) = [paragraph]
pageSkeletonParas (Image {}) = []

pageSkeletonLinks :: PageSkeleton -> [Link]
pageSkeletonLinks (Section _ _ children) = foldMap pageSkeletonLinks children
pageSkeletonLinks (Para (Paragraph _ bodies)) = foldMap paraBodyLinks bodies
pageSkeletonLinks (Image {}) = []

pageSectionPaths :: Page -> [SectionPath]
pageSectionPaths = map (\(path,_,_) -> path) . pageSections

pageSections :: Page -> [(SectionPath, [SectionHeading], [PageSkeleton])]
pageSections (Page {pageId=pageId, pageSkeleton=skel0}) =
    foldMap (pageSkeletonSections pageId) skel0

stubSections :: Stub -> [(SectionPath, [SectionHeading], [PageSkeleton])]
stubSections (Stub {stubPageId=pageId, stubSkeleton=skel0}) =
    foldMap (pageSkeletonSections pageId) skel0

pageSkeletonSections :: PageId -> PageSkeleton
                     -> [(SectionPath, [SectionHeading], [PageSkeleton])]
pageSkeletonSections pageId = go mempty mempty
  where
    go :: DList HeadingId -> DList SectionHeading
       -> PageSkeleton -> [(SectionPath, [SectionHeading], [PageSkeleton])]
    go parentIds parentHeadings (Section sectionName sectionId children) =
        let parentIds' = parentIds `DList.snoc` sectionId
            parentHeadings' = parentHeadings `DList.snoc` sectionName
            children' = filter (not . isSection) children
        in ( SectionPath pageId (DList.toList parentIds')
           , DList.toList parentHeadings'
           , children')
           : foldMap (go parentIds' parentHeadings') children
    go _ _ (Para {})  = []
    go _ _ (Image {}) = []

    isSection (Section {}) = True
    isSection _            = False

paraLinks :: Paragraph -> [Link]
paraLinks (Paragraph _ bodies) =
    foldMap paraBodyLinks bodies

paraBodyLinks :: ParaBody -> [Link]
paraBodyLinks (ParaText _text) = []
paraBodyLinks (ParaLink link)  = [link]

pageSkeletonText :: PageSkeleton -> [TL.Text]
pageSkeletonText (Section _ _ children) = foldMap pageSkeletonText children
pageSkeletonText (Para para) = [ paraToText para ]
pageSkeletonText (Image _ _) = []


pageFulltext :: Page -> [TL.Text]
pageFulltext (Page {pageName=pageName, pageSkeleton=skels}) =
    (TL.fromStrict $ getPageName pageName) : (foldMap pageSkeletonFulltext skels)

pageSkeletonFulltext :: PageSkeleton -> [TL.Text]
pageSkeletonFulltext (Section heading _ children) =
    (TL.fromStrict $ getSectionHeading heading) : (foldMap pageSkeletonFulltext children)
pageSkeletonFulltext (Para para) = [ paraToText para ]
pageSkeletonFulltext (Image _ children) =
    foldMap pageSkeletonFulltext children



paraToText :: Paragraph -> TL.Text
paraToText (Paragraph  _ bodies) =
    TL.concat $ fmap toText bodies
  where toText (ParaText text) = TL.fromStrict text
        toText (ParaLink link) = TL.fromStrict $ linkAnchor link

resolveRedirectFactory :: SiteId -> [Page] -> PageId -> PageId
resolveRedirectFactory siteId pages =
    resolveRedirectFun entityRedirects
  where
    !entityRedirects = inCompact $ entityRedirectMap siteId pages

resolveRedirectFun :: HM.HashMap PageId PageId -> PageId -> PageId
resolveRedirectFun entityRedirects origFromPageId = go mempty origFromPageId
  where
    go :: HS.HashSet PageId -> PageId -> PageId
    go history fromPageId
      | fromPageId `HS.member` history  = origFromPageId --  we are walking in circles, return original.
      | Just toPageId <- HM.lookup fromPageId entityRedirects = go (fromPageId `HS.insert` history)  toPageId  -- follow redirect
      | otherwise = fromPageId  -- success, we found a real page

entityRedirectMap :: SiteId -> [Page] -> HM.HashMap PageId PageId
entityRedirectMap siteId pages =
    HM.fromList $ mapMaybe extractRedirect $ pages
  where extractRedirect :: Page -> Maybe (PageId, PageId)
        extractRedirect page@(Page {pageId=fromPageId})
          | isNullPageId fromPageId = Nothing
          | otherwise = do
            toPageName <- pageRedirect page  -- MaybeMonad
            let toPageId = pageNameToId siteId toPageName
            guard $ not $ isNullPageId toPageId      -- if empty string -> Nothing
            pure (fromPageId, toPageId)

        isNullPageId :: PageId -> Bool
        isNullPageId = null . unpackPageId
