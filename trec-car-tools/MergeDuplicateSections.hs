{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}

import Data.Monoid hiding (All, Any)
import Options.Applicative
import qualified Data.HashSet as HS
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Text.PrettyPrint.ANSI.Leijen ((<$$>))
import Data.Maybe
import Data.Char
import Data.Void
import Control.Monad (void)
import Options.Applicative
import qualified Data.Binary.Serialise.CBOR as CBOR

import CAR.ToolVersion
import CAR.ToolVersion
import CAR.Types

helpDescr :: PP.Doc
helpDescr =
    "Merge duplicate section entries from a pages file"

opts :: Parser (FilePath, FilePath)
opts =
    (,)
    <$> argument str (help "input file" <> metavar "ANNOTATIONS FILE")
    <*> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")


main :: IO ()
main = do
    (inputFile, outputFile) <-
        execParser' 1 (helper <*> opts) (progDescDoc $ Just helpDescr)
    (prov, pages) <- readPagesFileWithProvenance inputFile

    let pages' =  fmap (recurseTransformPage mergeDuplicate) pages

    writeCarFile outputFile prov pages'


mergeDuplicate :: [PageSkeleton] -> [PageSkeleton]
mergeDuplicate sections =
      go sections
    where   go :: [PageSkeleton] -> [PageSkeleton]
            go ((Section heading sectionId children):tailSection) =
                let newChildren = concat
                       $ [ children'
                         | (Section heading' sectionId' children') <- tailSection
                         , sectionId' == sectionId
                         ]
                    remainingTail =
                         [ child
                         | child <- tailSection -- and paragraphs?
                         , includeChild sectionId child
                         ]

                in (Section heading sectionId (children ++ newChildren)) : go remainingTail

            go x = x

            includeChild :: HeadingId ->  PageSkeleton -> Bool
            includeChild sectionId (Section heading' sectionId' children') =
                sectionId /= sectionId'
            includeChild _ _ = True


recurseTransformPage :: ([PageSkeleton] -> [PageSkeleton]) -> Page -> Page
recurseTransformPage f page@(Page {..}) =
    page {pageSkeleton = f $ fmap (goSections f) pageSkeleton}


-- recurseFilterSections :: (PageSkeleton -> Bool) -> PageSkeleton -> Maybe PageSkeleton
-- recurseFilterSections f skel
--   | not $ f skel   = Nothing                   -- here we do the actual filtering
-- recurseFilterSections f (Section heading sectionId children)
--   | null children' = Nothing
--   | otherwise      = Just (Section heading sectionId children')   -- recurse
--   where children' = mapMaybe (recurseFilterSections f) children
-- recurseFilterSections _ skel = Just skel                          -- keep

goSections :: ([PageSkeleton] -> [PageSkeleton]) -> PageSkeleton -> PageSkeleton
goSections f section@(Section heading sectionId children) =
    let children'' :: [PageSkeleton]
        children'' = fmap (goSections f) children
    in (Section heading sectionId (f children''))

goSections f skel = skel



--
--
-- recurseTransformSections :: ([PageSkeleton] -> [PageSkeleton]) -> [PageSkeleton] -> [PageSkeleton]
-- recurseTransformSections f skels =
--      [ go skel | skel <- skels  ]
--   where go f skel =
--
--
--   | null children' = children'
--   | otherwise      = f (Section heading sectionId children')   -- recurse
--   where children' = (recurseTransformSections f) children
-- recurseTransformSections _ skel = skel
