{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ApplicativeDo #-}

module CAR.FillMetadata  where

import qualified Control.Foldl as Foldl
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Semigroup hiding (option)
import Data.Foldable (foldl')
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Coerce
import Data.Vector.Algorithms.Intro
import Data.Ord

import CAR.Utils
import CAR.Types
import CAR.Utils.Redirects


-- action for resolving redirects for all pages in inputPath
stageResolveRedirect :: FilePath -> IO (Provenance, [Page])
stageResolveRedirect inputPath = do
    redirectResolver <- resolveRedirectsWithSection <$> readPagesFile inputPath

    let theFold = ((,) <$> buildPageNameMap) <*> buildRedirectMap
    (pageNameMap, redirectMap) <- Foldl.fold theFold <$> readPagesFile inputPath

    (prov, pages) <- readPagesFileWithProvenance inputPath
    let pageNameResolver :: PageId -> Maybe PageName
        pageNameResolver = flip HM.lookup pageNameMap
    let pages' = map (fixLinks redirectResolver pageNameResolver . fillRedirectMetadata redirectMap) pages
    return (prov, pages')



-- action for resolving disambiguation names and inlinks
stageResolveDisambiguationAndInlinks :: FilePath -> IO (Provenance, [Page])
stageResolveDisambiguationAndInlinks inputPath = do
    acc <- unionsWith (<>) . fmap buildDisambiguateInlinksMap <$> readPagesFile inputPath
    (prov, pages) <- readPagesFileWithProvenance inputPath
    let pages' = map (fillDisambigInlinkMetadata acc) pages
    return (prov, pages')


-- action for loading category tags into metadata
stageResolveCategoryTags :: FilePath -> IO (Provenance, [Page])
stageResolveCategoryTags inputPath = do
    allCategoryIds <- extractAllCategoryIds <$> readPagesFile inputPath
    acc <- unionsWith (<>) . fmap (buildCategoryMap allCategoryIds) <$> readPagesFile inputPath
    (prov, pages) <- readPagesFileWithProvenance inputPath
    let pages' = map (fillCategoryMetadata acc) pages
    return (prov, pages')


fixLinks :: (PageId -> (PageId, Maybe T.Text)) -> (PageId -> Maybe PageName) -> Page -> Page
fixLinks redirectResolver pageNameResolver page =
    page {pageSkeleton = fmap goSkeleton (pageSkeleton  page)}
      where
        goSkeleton (Section x y children) = Section x y (fmap goSkeleton children)
        goSkeleton (Para p)        = Para (goParagraph p)
        goSkeleton (Image x skel)  = Image x (fmap goSkeleton skel)
        goSkeleton (List x p)      = List x (goParagraph p)
        goSkeleton (Infobox tag p) = Infobox tag $ map (fmap $ map goSkeleton) p

        goParagraph (Paragraph x bodies) = Paragraph x (fmap goParaBody bodies)

        goParaBody (ParaText t) = ParaText t
        goParaBody (ParaLink l) =
            case pageNameResolver newLinkTargetId of
              Just newLinkTargetName ->
                case maybeSection of
                    -- redirect with Section
                    Just section ->
                        ParaLink l { linkTarget = newLinkTargetName
                                   , linkTargetId = newLinkTargetId
                                   , linkSection = Just section
                                   }
                    -- redirect without section, respect section of this link
                    Nothing ->
                        ParaLink l { linkTarget = newLinkTargetName
                                   , linkTargetId = newLinkTargetId
                                   }
              -- In cases where the target page does not exist simply drop the link
              Nothing -> ParaText (linkAnchor l)
          where (newLinkTargetId, maybeSection) = redirectResolver (linkTargetId l)


-- Todo  pageNameResolver: package up and move to utils
buildPageNameMap :: Foldl.Fold Page (HM.HashMap PageId PageName)
buildPageNameMap =
    Foldl.Fold (\acc page -> HM.insert (pageId page) (pageName page) acc) mempty id

-- Assume that category page ids are already filled in from the import stage;
-- otherwise call buildCategoryMap

-- | Map of pages that redirect to the given page.
newtype RedirectMap = RedirectMap (HM.HashMap PageId (HS.HashSet PageName))

-- | Collect a map of the in-bound redirect links
buildRedirectMap :: Foldl.Fold Page RedirectMap
buildRedirectMap =
    Foldl.Fold step mempty RedirectMap
  where
    step acc page
      | RedirectPage l <- pageType page
      = HM.insertWith (<>) (linkTargetId l) (HS.singleton (pageName page)) acc
      | otherwise = acc

-- Assume that category page ids are already filled in from the inport stage;  otherwise call buildCategoryMap
-- Also assume that redirect are already resolved
buildDisambiguateInlinksMap :: Page -> HM.HashMap PageId Acc
buildDisambiguateInlinksMap page =
    foldl' (HM.unionWith (<>)) mempty (disambigs <> inlinkIds <> inlinkAnchors)
  where
    disambigs =
        [ HM.singleton (linkTargetId link)
          $ mempty { accDisambigNames = HS.singleton (pageName page)
                   , accDisambigIds = HS.singleton (pageId page) }
        | DisambiguationPage <- pure $ pageType page
        , link <- listItemFirstLinks page
        ]
    inlinkIds =
        [ HM.singleton (linkTargetId link)
          $ mempty { accInlinkIds = HS.singleton (pageId page) }
        | pageIsArticle page || pageIsCategory page
        , link <- pageLinks page
        ]
    inlinkAnchors =
        [ HM.singleton (linkTargetId link)
          $ mempty { accInlinkAnchors = HM.singleton (linkAnchor link) (Sum 1) }
        | pageIsArticle page || pageIsCategory page
        , link <- pageLinks page
        ]

    -- | For disambiguation names/ids: only take the first link of every list item.
    listItemFirstLinks :: Page -> [Link]
    listItemFirstLinks = foldMap listItemFirstskeletonLinks . pageSkeleton

    listItemFirstskeletonLinks :: PageSkeleton -> [Link]
    listItemFirstskeletonLinks (Section _ _ children) = foldMap listItemFirstskeletonLinks children
    listItemFirstskeletonLinks (Para (Paragraph _ _)) = []
    listItemFirstskeletonLinks (Image {}) = []
    listItemFirstskeletonLinks (List _ (Paragraph _ bodies)) =
        case foldMap paraBodyLinks bodies of
        [] -> []
        (a:_) -> [a]
    listItemFirstskeletonLinks (Infobox {}) = []

extractAllCategoryIds :: [Page] -> HS.HashSet PageId
extractAllCategoryIds pages  =
    HS.fromList
    $ map pageId
    $ filter pageIsCategory pages

buildCategoryMap :: HS.HashSet PageId -- ^ set of known categories
         -> Page -> HM.HashMap PageId Acc
buildCategoryMap allCategoryIds page =
    foldl' (HM.unionWith (<>)) mempty [categories]
  where
    categories =
        HM.singleton (pageId page)
        $ mempty { accCategoryNames = HS.fromList $ map linkTarget categoryLinks
                 , accCategoryIds = HS.fromList $ map linkTargetId categoryLinks
                 }
      where
        categoryLinks =
            [ link
            | link <- pageLinks page
            , linkTargetId link `HS.member` allCategoryIds
            ]

data Acc = Acc { accRedirectNames :: !(HS.HashSet PageName)
               , accDisambigNames :: !(HS.HashSet PageName)
               , accDisambigIds   :: !(HS.HashSet PageId)
               , accCategoryNames :: !(HS.HashSet PageName)
               , accCategoryIds   :: !(HS.HashSet PageId)
               , accInlinkIds     :: !(HS.HashSet PageId)
               , accInlinkAnchors :: !(HM.HashMap T.Text (Sum Int))
               }

instance Monoid Acc where
    mempty = Acc m m m m m m m
      where
        m :: Monoid m => m
        m = mempty
    mappend = (<>)

instance Semigroup Acc where
    Acc a1 b1 c1 d1 e1 f1 g1 <> Acc a2 b2 c2 d2 e2 f2 g2 =
        Acc (a1<>a2) (b1<>b2) (c1<>c2) (d1<>d2) (e1<>e2) (f1<>f2) (HM.unionWith (<>) g1 g2)

fillDisambigInlinkMetadata :: HM.HashMap PageId Acc -> Page -> Page
fillDisambigInlinkMetadata acc page =
    page { pageMetadata =
               setMetadata _DisambiguationNames (HS.toList $ accDisambigNames things)
               $ setMetadata _DisambiguationIds (HS.toList $ accDisambigIds things)
               $ setMetadata _InlinkIds (HS.toList $ accInlinkIds things)
               $ setMetadata _InlinkAnchors inlinkAnchors
               $ pageMetadata page
         }
  where
    inlinkAnchors :: V.Vector (T.Text, Int)
    inlinkAnchors =
        V.modify (sortBy $ comparing $ Down . snd)
        $ coerce
        $ V.fromListN (HM.size $ accInlinkAnchors things)
        $ HM.toList $ accInlinkAnchors things
    things = fromMaybe mempty $ HM.lookup (pageId page) acc



fillRedirectMetadata :: RedirectMap -> Page -> Page
fillRedirectMetadata (RedirectMap acc) page =
    page { pageMetadata = setMetadata _RedirectNames (HS.toList things)
                          $ pageMetadata page
         }
  where
    things = fromMaybe mempty $ HM.lookup (pageId page) acc


fillCategoryMetadata :: HM.HashMap PageId Acc -> Page -> Page
fillCategoryMetadata acc page =
    page { pageMetadata =
               setMetadata _CategoryNames (HS.toList $ accCategoryNames things)
               $ setMetadata _CategoryIds (HS.toList $ accCategoryIds things)
               $ pageMetadata page
         }
  where
    things = fromMaybe mempty $ HM.lookup (pageId page) acc
