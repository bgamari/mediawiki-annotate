module CAR.Utils where

import Data.Maybe
import qualified Data.Text as T
import CAR.Types

pageIsRedirect :: Page -> Bool
pageIsRedirect (Page {pageSkeleton=[Para (Paragraph _ (ParaText t:_))]}) =
    T.pack "#redirect" `T.isPrefixOf` T.toCaseFold (T.stripStart t)
pageIsRedirect _ = False

pageIsDisambiguation :: Page -> Bool
pageIsDisambiguation (Page { pageName = PageName t }) =
    (T.pack " (disambiguation)") `T.isInfixOf` t



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
pageLinkTargets = foldMap skeletonLinks . pageSkeleton
  where
    skeletonLinks :: PageSkeleton -> [PageName]
    skeletonLinks (Section _ _ children) = foldMap skeletonLinks children
    skeletonLinks (Para (Paragraph _ bodies)) = foldMap paraBodyLinks bodies

paraBodyLinks :: ParaBody -> [PageName]
paraBodyLinks (ParaText text) = []
paraBodyLinks (ParaLink (PageName target) _) = [normTarget]
  where normTarget = PageName $ normFirst $ T.takeWhile (/= '#') target
          where normFirst link = (\(a,b) -> T.toUpper a `T.append` b) $ T.splitAt 1 link

paraToText :: Paragraph -> T.Text
paraToText (Paragraph  _ bodies) =
    T.concat $ fmap toText bodies
  where toText (ParaText text) = text
        toText (ParaLink _ text) = text

paraToLinks :: Paragraph -> [PageName]
paraToLinks (Paragraph _ bodies) =
    foldMap paraBodyLinks bodies
