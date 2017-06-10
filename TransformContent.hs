{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Monoid hiding (All, Any)
import System.IO
import Options.Applicative
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as BSB
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Text.PrettyPrint.ANSI.Leijen ((<$$>))
import Data.Maybe
import Data.Char

import CAR.Types


helpDescr :: PP.Doc
helpDescr =
    "Predicate options:" <$$> PP.indent 4 options
  where
    cmd a b = PP.nest 8 (a <$$> b)
    options = PP.vsep
      [
        cmd "need doc"                           "here"
      ]

opts :: Parser (FilePath, FilePath, Page -> Maybe Page)
opts =
    (,,)
    <$> argument str (help "input file" <> metavar "ANNOTATIONS FILE")
    <*> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")
    <*> (fullMode <|> sectionsCategoriesMode)
  where 
    fullMode = flag' fullTransformContent (long "full" <> help "full filtering of lead, category, hidden sections, and short page remainders")
    sectionsCategoriesMode = flag' transformCategoriesAndForbiddenSection  (long "sections-categories" <> help "partial filtering of category and hidden sections only")

    -- <*> argument predicate (metavar "PRED" <> help "Predicate")

forbiddenHeadings :: HS.HashSet T.Text
forbiddenHeadings = HS.fromList ["see also", "references", "external links", "notes",
    "bibliography", "gallery", "publications", "further reading", "track listing", "sources",
    "cast", "discography", "awards", "other"]

isPara :: PageSkeleton -> Bool
isPara (Para{})    = True
isPara (Section{}) = False
isPara (Image{})   = False

isImage :: PageSkeleton -> Bool
isImage (Para{})    = False
isImage (Section{}) = False
isImage (Image{})   = True


isForbiddenSkeleton :: PageSkeleton -> Bool
isForbiddenSkeleton (Section heading _ _) =
    ( T.toCaseFold (getSectionHeading heading) `HS.member` forbiddenHeadings )  ||  -- excluded
    T.length (T.filter isAlpha (getSectionHeading heading)) < 3 ||                      -- not enough letters
    T.length (getSectionHeading heading) > 100                                          -- too long, probably parse error
isForbiddenSkeleton _ = False

-- recurseDropForbiddenSections =  recurseSections (not . isForbiddenSkeleton)

recurseFilterSections :: (PageSkeleton -> Bool) -> PageSkeleton -> Maybe PageSkeleton
recurseFilterSections f (Section heading sectionId children)
    | null children' = Nothing
    | otherwise =  Just (Section heading sectionId children')
  where children' = mapMaybe (recurseFilterSections f) $ filter f children
recurseFilterSections f x = Just x


recurseFilterPage :: (PageSkeleton -> Bool) -> Page -> Page
recurseFilterPage f (Page {..}) =
    Page pageName pageId children'
  where children' = mapMaybe (recurseFilterSections f) pageSkeleton

isCategoriesPara :: PageSkeleton -> Bool
isCategoriesPara (Para (Paragraph paraId paraBody)) =
    all isCategoryLinks paraBody
  where
    isCategoryLinks :: ParaBody -> Bool
    isCategoryLinks (ParaText _) = False
    isCategoryLinks (ParaLink (Link (PageName name) section _ anchor)) =
      "Category:" `T.isPrefixOf` name
isCategoriesPara _ = False

-- transformOnlySections :: Page -> Page
-- transformOnlySections (Page {pageName, pageId, pageSkeleton=pageSkeleta}) =
--     Page pageName pageId pageSkeleta'
--   where
--     pageSkeleta'=
--          mapMaybe recurseDropForbiddenSections
--          $ filter (\skel -> not (isForbiddenSkeleton skel || isCategoriesPara skel)) pageSkeleta
--
-- transformContent :: Page -> Maybe Page
-- transformContent (Page {pageName, pageId, pageSkeleton=pageSkeleta})
--   | length pageSkeleta' > 3 =
--       Just (Page pageName pageId pageSkeleta')
--   | otherwise = Nothing
--   where
--     pageSkeleta'=
--          mapMaybe recurseDropForbiddenSections
--          $ filter (\skel -> not (isForbiddenSkeleton skel) &&  not (isLead skel))
--            pageSkeleta

---

--
-- transformForbiddenSections :: Page -> Page
-- transformForbiddenSections (Page {pageName, pageId, pageSkeleton=pageSkeleta}) =
--     Page pageName pageId pageSkeleta'
--   where
--     pageSkeleta'=
--          mapMaybe recurseDropForbiddenSections
--          $ filter (\skel -> not (isForbiddenSkeleton skel))
--            pageSkeleta
--
--
--
-- transformDropLead :: Page -> Page
-- transformDropLead (Page {pageName, pageId, pageSkeleton=pageSkeleta}) =
--     Page pageName pageId pageSkeleta'
--   where
--     pageSkeleta'=
--          filter (\skel -> not (isLead skel))
--            pageSkeleta


filterTopLevel :: (PageSkeleton -> Bool) -> Page  ->  Page
filterTopLevel f (Page {pageName, pageId, pageSkeleton=pageSkeleta}) =
    Page pageName pageId (filter f pageSkeleta)



dropShortPage :: Page -> Maybe Page
dropShortPage page@(Page {pageName, pageId, pageSkeleton=pageSkeleta})
  | length pageSkeleta > 3 =
      Just page
  | otherwise = Nothing
  

fullTransformContent :: Page -> Maybe Page
fullTransformContent page =
    dropShortPage
    $ recurseFilterPage (not . isForbiddenSkeleton)
    $ recurseFilterPage (not . isImage)
    $ recurseFilterPage (not . isCategoriesPara)
    $ filterTopLevel (not . isPara)
    page



transformCategoriesAndForbiddenSection :: Page -> Maybe Page
transformCategoriesAndForbiddenSection page =
    Just
    $ recurseFilterPage (not . isForbiddenSkeleton)
    $ recurseFilterPage (not . isImage)
    $ recurseFilterPage (not . isCategoriesPara)
    page

---

main :: IO ()
main = do
    (inputFile, outputFile, transformMode) <- execParser $ info (helper <*> opts) (progDescDoc $ Just helpDescr)
    pages <- decodeCborList <$> BSL.readFile inputFile
    writeCborList outputFile $ mapMaybe transformMode pages

