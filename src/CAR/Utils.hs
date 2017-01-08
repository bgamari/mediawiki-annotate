module CAR.Utils where

import Data.Maybe
import qualified Data.Text as T
import CAR.Types

pageIsRedirect :: Page -> Bool
pageIsRedirect (Page {pageSkeleton=[Para (Paragraph _ (ParaText t:_))]}) =
    T.pack "#redirect" `T.isPrefixOf` T.toCaseFold (T.stripStart t)
pageIsRedirect _ = False

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
    paraBodyLinks (ParaLink pageName _) = [pageName]
    paraBodyLinks (ParaText _ ) = []
