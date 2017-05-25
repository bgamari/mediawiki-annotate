{-# LANGUAGE RecordWildCards #-}

module CAR.Utils where

import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import CAR.Types


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
    goSkeleton (Image _ caption) = any goSkeleton caption

    goParaBody (ParaLink l) = str `T.isInfixOf` linkAnchor l
    goParaBody (ParaText t) = str `T.isInfixOf` t

pageCategories :: Page -> [T.Text]
pageCategories = mapMaybe isCategoryTag . pageLinkTargets
  where
    isCategoryTag :: PageName -> Maybe T.Text
    isCategoryTag (PageName pageName) =
        T.pack "Category:" `T.stripPrefix` pageName

pageLinkTargets :: Page -> [PageName]
pageLinkTargets = map linkTarget . pageLinks

pageLinks :: Page -> [Link]
pageLinks = foldMap pageSkeletonLinks . pageSkeleton

pageSkeletonLinks :: PageSkeleton -> [Link]
pageSkeletonLinks (Section _ _ children) = foldMap pageSkeletonLinks children
pageSkeletonLinks (Para (Paragraph _ bodies)) = foldMap paraBodyLinks bodies
pageSkeletonLinks (Image _ caption) = foldMap pageSkeletonLinks caption

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

paraToText :: Paragraph -> TL.Text
paraToText (Paragraph  _ bodies) =
    TL.concat $ fmap toText bodies
  where toText (ParaText text) = TL.fromStrict text
        toText (ParaLink link) = TL.fromStrict $ linkAnchor link

