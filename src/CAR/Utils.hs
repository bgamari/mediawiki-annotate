module CAR.Utils where

import Data.Hashable
import qualified Data.DList as DList
import           Data.DList (DList)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Foldable
import CAR.Types


unionsWith :: (Foldable g, Eq k, Hashable k)  => (v -> v -> v) -> g (HM.HashMap k v) -> HM.HashMap k v
unionsWith f = foldl' (HM.unionWith f) mempty


-- | Identify the target of a redirect page.
pageRedirect :: Page -> Maybe Link
pageRedirect Page { pageMetadata = meta }
  | RedirectPage l <- pagemetaType meta = Just l
  | otherwise = Nothing

-- | True if this is a disambiguation page (language-specifics already resolved)
pageIsDisambiguation :: Page -> Bool
pageIsDisambiguation (Page { pageMetadata = meta })
  | DisambiguationPage <- pagemetaType meta = True
  | otherwise = False

pageIsCategory :: Page -> Bool
pageIsCategory (Page { pageMetadata = meta })
  | CategoryPage <- pagemetaType meta = True
  | otherwise = False

pageIsArticle :: Page -> Bool
pageIsArticle (Page { pageMetadata = meta })
  | ArticlePage <- pagemetaType meta = True
  | otherwise = False

pageContainsText :: T.Text -> Page -> Bool
pageContainsText str = any goSkeleton . pageSkeleton
  where
    goSkeleton (Section _ _ children) = any goSkeleton children
    goSkeleton (Para p) = goParagraph p
    goSkeleton (Image {}) = False
    goSkeleton (List _ p) = goParagraph p

    goParagraph (Paragraph _ bodies) = any goParaBody bodies

    goParaBody (ParaLink l) = str `T.isInfixOf` linkAnchor l
    goParaBody (ParaText t) = str `T.isInfixOf` t

pageLinkTargets :: Page -> [PageName]
pageLinkTargets = map linkTarget . pageLinks

pageLinkTargetIds :: Page -> [PageId]
pageLinkTargetIds = map linkTargetId . pageLinks

pageLinks :: Page -> [Link]
pageLinks = foldMap pageSkeletonLinks . pageSkeleton

pageParas :: Page -> [Paragraph]
pageParas = foldMap pageSkeletonParas . pageSkeleton

pageSkeletonParas :: PageSkeleton -> [Paragraph]
pageSkeletonParas (Section _ _ children) = foldMap pageSkeletonParas children
pageSkeletonParas (Para paragraph) = [paragraph]
pageSkeletonParas (Image {}) = []
pageSkeletonParas (List _ paragraph) = [paragraph]

pageSkeletonLinks :: PageSkeleton -> [Link]
pageSkeletonLinks (Section _ _ children) = foldMap pageSkeletonLinks children
pageSkeletonLinks (Para (Paragraph _ bodies)) = foldMap paraBodyLinks bodies
pageSkeletonLinks (Image {}) = []
pageSkeletonLinks (List _ (Paragraph _ bodies)) = foldMap paraBodyLinks bodies

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
    go _ _ (List {})  = []

    isSection (Section {}) = True
    isSection _            = False

paraLinks :: Paragraph -> [Link]
paraLinks (Paragraph _ bodies) =
    foldMap paraBodyLinks bodies

paraBodyLinks :: ParaBody -> [Link]
paraBodyLinks (ParaText _text) = []
paraBodyLinks (ParaLink link)  = [link]

-- | Returns all visible text (including headers, page titles, and captions)  of the page.
pageFulltext :: Page -> [TL.Text]
pageFulltext (Page {pageName=pageName, pageSkeleton=skels}) =
    (TL.fromStrict $ getPageName pageName) : (foldMap pageSkeletonFulltext skels)


-- | Returns all visible text (including headers, etc ) from the page skeleton.
pageSkeletonFulltext :: PageSkeleton -> [TL.Text]
pageSkeletonFulltext (Section heading _ children) =
    (TL.fromStrict $ getSectionHeading heading) : (foldMap pageSkeletonFulltext children)
pageSkeletonFulltext (Para para) = [paraToText para]
pageSkeletonFulltext (Image _ children) =
    foldMap pageSkeletonFulltext children
pageSkeletonFulltext (List _ para) = [paraToText para]

paraToText :: Paragraph -> TL.Text
paraToText (Paragraph  _ bodies) =
    TL.concat $ fmap toText bodies
  where toText (ParaText text) = TL.fromStrict text
        toText (ParaLink link) = TL.fromStrict $ linkAnchor link
