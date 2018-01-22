module CAR.Utils where

import Data.Hashable
import qualified Data.DList as DList
import           Data.DList (DList)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Set as S
import Data.List
import CAR.Types


unionsWith :: (Foldable g, Eq k, Hashable k)  => (v -> v -> v) -> g (HM.HashMap k v) -> HM.HashMap k v
unionsWith f = foldl' (HM.unionWith f) mempty


-- | Identify the target of a redirect page.
pageRedirect :: Page -> Maybe Link
pageRedirect Page {pageType = RedirectPage l} = Just l
pageRedirect _ = Nothing

-- | True if this is a disambiguation page (language-specifics already resolved)
pageIsDisambiguation :: Page -> Bool
pageIsDisambiguation (Page { pageType = DisambiguationPage }) = True
pageIsDisambiguation _ = False

pageIsCategory :: Page -> Bool
pageIsCategory (Page { pageType = CategoryPage }) = True
pageIsCategory _ = False

pageIsArticle :: Page -> Bool
pageIsArticle (Page { pageType = ArticlePage }) = True
pageIsArticle _ = False

pageContainsText :: T.Text -> Page -> Bool
pageContainsText str = any goSkeleton . pageSkeleton
  where
    goSkeleton (Section _ _ children) = any goSkeleton children
    goSkeleton (Para p) = goParagraph p
    goSkeleton (Image {}) = False
    goSkeleton (List _ p) = goParagraph p
    goSkeleton (Infobox tag args) = any (any goSkeleton . snd) args

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
pageSkeletonParas (Infobox tag args) = foldMap (foldMap pageSkeletonParas . snd) args

pageSkeletonLinks :: PageSkeleton -> [Link]
pageSkeletonLinks (Section _ _ children) = foldMap pageSkeletonLinks children
pageSkeletonLinks (Para (Paragraph _ bodies)) = foldMap paraBodyLinks bodies
pageSkeletonLinks (Image {}) = []
pageSkeletonLinks (List _ (Paragraph _ bodies)) = foldMap paraBodyLinks bodies
pageSkeletonLinks (Infobox tag args) = foldMap (foldMap pageSkeletonLinks . snd) args

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
pageSkeletonFulltext (Infobox _ args) = foldMap (foldMap pageSkeletonFulltext . snd) args

paraToText :: Paragraph -> TL.Text
paraToText (Paragraph  _ bodies) =
    TL.concat $ fmap toText bodies
  where toText (ParaText text) = TL.fromStrict text
        toText (ParaLink link) = TL.fromStrict $ linkAnchor link


nubWithKey :: Ord b => (a -> b) -> [a] -> [a]
nubWithKey f list = go mempty list
  where
    go seen (x:xs)
      | (f x) `S.member` seen = go seen xs
      | otherwise             = x : go (S.insert (f x) seen) xs
    go _ [] = []

