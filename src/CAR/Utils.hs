module CAR.Utils where

import Data.Maybe
import qualified Data.Text as T
import CAR.Types

pageRedirect :: Page -> Maybe PageName
pageRedirect (Page {pageSkeleton=Para (Paragraph _ (ParaText t : rest)) : _})
  | T.pack "#redirect" `T.isPrefixOf` T.toCaseFold (T.stripStart t)
  , (ParaLink t _) : _ <- rest = Just t
pageRedirect _ = Nothing

pageIsDisambiguation :: Page -> Bool
pageIsDisambiguation (Page { pageName = PageName t }) =
    (T.pack " (disambiguation)") `T.isInfixOf` T.toCaseFold t



pageContainsText :: T.Text -> Page -> Bool
pageContainsText str = any goSkeleton . pageSkeleton
  where
    goSkeleton (Section _ _ children) = any goSkeleton children
    goSkeleton (Para (Paragraph _ bodies)) = any goParaBody bodies

    goParaBody (ParaLink _ t) = str `T.isInfixOf` t
    goParaBody (ParaText t)   = str `T.isInfixOf` t

pageCategories :: Page -> [T.Text]
pageCategories = mapMaybe isCategoryTag . pageLinkTargets
  where
    isCategoryTag :: PageName -> Maybe T.Text
    isCategoryTag (PageName pageName) =
        T.pack "Category:" `T.stripPrefix` pageName

pageLinkTargets :: Page -> [PageName]
pageLinkTargets = map fst . pageLinks

pageLinks :: Page -> [(PageName, T.Text)]
pageLinks = foldMap pageSkeletonLinks . pageSkeleton

pageSkeletonLinks :: PageSkeleton -> [(PageName, T.Text)]
pageSkeletonLinks (Section _ _ children) = foldMap pageSkeletonLinks children
pageSkeletonLinks (Para (Paragraph _ bodies)) = foldMap paraBodyLinks bodies

paraLinks :: Paragraph -> [(PageName, T.Text)]
paraLinks (Paragraph _ bodies) =
    foldMap paraBodyLinks bodies

paraBodyLinks :: ParaBody -> [(PageName, T.Text)]         -- todo T.Text should be PageName
paraBodyLinks (ParaText text) = []
paraBodyLinks (ParaLink target anchor) = [(normTargetPageName target, anchor)]


normTargetPageName :: PageName -> PageName -- todo move normalization to import
normTargetPageName (PageName target) =
    PageName $ normFirst $ T.takeWhile (/= '#') target
  where normFirst link = (\(a,b) -> T.toUpper a `T.append` b) $ T.splitAt 1 link

pageSkeletonText :: PageSkeleton -> [T.Text]
pageSkeletonText (Section _ _ children) = foldMap pageSkeletonText children
pageSkeletonText (Para para) = [ paraToText para ]

paraToText :: Paragraph -> T.Text
paraToText (Paragraph  _ bodies) =
    T.concat $ fmap toText bodies
  where toText (ParaText text) = text
        toText (ParaLink _ text) = text
