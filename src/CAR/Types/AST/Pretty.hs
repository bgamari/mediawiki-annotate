module CAR.Types.AST.Pretty
    ( -- * Printing
      prettyPage
    , prettySkeleton
    , prettyParagraph
      -- ** Link styles
    , LinkStyle
    , withLink, anchorOnly
    ) where

import Data.Semigroup
import Data.List
import qualified Data.Text as T
import CAR.Types.AST

prettyPage :: LinkStyle -> Page -> String
prettyPage linkStyle (Page (PageName name) pageId pageType metaData skeleton) =
    unlines $ [ T.unpack name
              , replicate (T.length name) '='
              , ""
              , "   " ++ show pageId  ++ " ("++show pageType ++") "
              , "   " ++ show metaData
              , ""
              ]
            ++ map (prettySkeleton linkStyle) skeleton

prettySkeleton :: LinkStyle -> PageSkeleton -> String
prettySkeleton renderLink = go 1
  where
    go :: Int -> PageSkeleton -> String
    go n (Section (SectionHeading name) _ children) =
        unlines
        $ [ replicate n '#' ++ " " ++ T.unpack name]
          <> map (go (n+1)) children
          <> [""]
    go _ (Para para) = prettyParagraph renderLink para
    go _ (Image target children) =
        "![" ++ unlines (map (go 1) children) ++ "](" ++ T.unpack target ++ ")"
    go _ (List n para) = replicate n '*' ++ " " ++ prettyParagraph renderLink para
    go _ (Infobox tag args) = "{{"<>T.unpack tag<>"|"<>pairs<>"}}"
      where
        pairs = intercalate "," $ map (\(k,v) -> T.unpack k <> "=" <> foldMap (go 0) v) args

prettyParagraph :: LinkStyle -> Paragraph -> String
prettyParagraph renderLink (Paragraph pid bodies) =
    "{" ++ unpackParagraphId pid ++ "} " ++ concatMap go bodies ++ "\n"
  where
    go (ParaText t) = T.unpack t
    go (ParaLink l) = renderLink l

type LinkStyle = Link -> String

withLink :: LinkStyle
withLink (Link (PageName name) section _ anchor) =
    "["<>T.unpack anchor<>"]("<>T.unpack name<>msection<>")"
  where
    msection
      | Just s <- section = "#"<>T.unpack s
      | otherwise         = mempty

anchorOnly :: LinkStyle
anchorOnly l = T.unpack $ linkAnchor l
