{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}

import Data.Monoid hiding (All, Any)
import Options.Applicative
import qualified Data.HashSet as HS
import qualified Data.Text as T
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

data ConfOpts = ConfOpts { forbiddenHeadings :: HS.HashSet T.Text
                         , removeStdHeadings :: Bool
                         , includeLead :: Bool
                         , includeImage :: Bool
                         , includeCategories :: Bool
                         , includeShortHeadings :: Bool
                         , includeLongHeadings :: Bool
                         , includeShortPage :: Bool
                         }

opts :: Parser (FilePath, FilePath, Page -> Maybe Page)
opts =
    (,,)
    <$> argument str (help "input file" <> metavar "ANNOTATIONS FILE")
    <*> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")
    <*> (fullMode <|> sectionsCategoriesMode <|>  configurableMode)
  where
    fullMode = flag' transformFull (long "full" <> help "full  filtering of lead, category, hidden sections, and short page remainders")
    sectionsCategoriesMode = flag' transformCategoriesAndForbiddenSection  (long "sections-categories" <> help "partial filtering of category and hidden sections only")
    configurableMode = transformConf <$> (do includeLead  <- switch (long "lead" <> help "include lead paragraph")
                                             includeImage <- switch (long "image" <> help "include images")
                                             includeCategories <- switch (long "category" <> help "include categories")
                                             forbiddenHeadings <- HS.fromList <$> many (option (T.pack <$> str) (long "forbidden" <> metavar "HEADING" <> help "remove this heading from outline (add multiple times)"))
                                             removeStdHeadings <- switch (long "forbiddenDefaultHeadings" <> help "remove default forbidden headings ")
                                             includeShortHeadings <-switch (long "shortHeading" <> help "include headings if less than 3 non-alpha characters")
                                             includeLongHeadings <-switch (long "longHeading" <> help "include headings if they are longer than 100 characters")

                                             includeShortPage <- switch (long "shortpage" <> help "keep pages that contain less than three sections after filtering")
                                             return ConfOpts {..}
                                         )
    transformConf :: ConfOpts -> Page -> Maybe Page
    transformConf ConfOpts{..} page =

        let funs = catMaybes
                   [ Just                             $ recurseFilterPage (not . isForbiddenHeading forbiddenHeadings)
                   , deactivate removeStdHeadings     $ recurseFilterPage (not . isForbiddenHeading forbiddenStdHeadings)
                   , deactivate includeShortHeadings  $ recurseFilterPage (not . isShortHeading)
                   , deactivate includeLongHeadings   $ recurseFilterPage (not . isLongHeading)
                   , deactivate includeImage          $ recurseFilterPage (not . isImage)
                   , deactivate includeCategories     $ recurseFilterPage (not . isCategoriesPara)
                   , deactivate includeLead           $ topLevelFilterPage (not . isPara)
                   ]
            deactivate :: Bool -> a -> Maybe a
            deactivate False x = Just x
            deactivate True _ = Nothing

                  
        in (if includeShortPage then Just else dropPageIfShort)
           $ foldr (.) id funs $ page


    -- <*> argument predicate (metavar "PRED" <> help "Predicate")

forbiddenStdHeadings :: HS.HashSet T.Text
forbiddenStdHeadings = HS.fromList ["see also", "references", "external links", "notes",
    "bibliography", "gallery", "publications", "further reading", "track listing", "sources",
    "cast", "discography", "awards", "other",
    "external links and references", "notes and references" ] --new in v1.6

isPara :: PageSkeleton -> Bool
isPara (Para{}) = True
isPara _        = False

isImage :: PageSkeleton -> Bool
isImage (Image{})   = True
isImage _           = False




isForbiddenHeading ::HS.HashSet T.Text -> PageSkeleton -> Bool
isForbiddenHeading forbiddenHeadings' (Section heading _ _) =
    ( T.toCaseFold (getSectionHeading heading) `HS.member` forbiddenHeadings' )  -- excluded
isForbiddenHeading _ _ = False


isShortHeading :: PageSkeleton -> Bool
isShortHeading (Section heading _ _) =
    T.length (T.filter isAlpha (getSectionHeading heading)) < 3                      -- not enough letters
isShortHeading _ = False

isLongHeading :: PageSkeleton -> Bool
isLongHeading (Section heading _ _) =
    T.length (getSectionHeading heading) > 100                                          -- too long, probably parse error
isLongHeading _ = False



-- recurseDropForbiddenSections =  recurseSections (not . isForbiddenOrShortOrLongHeading)

recurseFilterSections :: (PageSkeleton -> Bool) -> PageSkeleton -> Maybe PageSkeleton
recurseFilterSections f skel
  | not $ f skel   = Nothing                   -- here we do the actual filtering
recurseFilterSections f (Section heading sectionId children)
  | null children' = Nothing
  | otherwise      = Just (Section heading sectionId children')   -- recurse
  where children' = mapMaybe (recurseFilterSections f) children
recurseFilterSections _ skel = Just skel                          -- keep
-- this implementation is making use of fall-through rules
-- first see whether filter applies
-- if fall through, then recurse to children
-- if all else fails retain



recurseFilterPage :: (PageSkeleton -> Bool) -> Page -> Page
recurseFilterPage f page@(Page {..}) =
    page {pageSkeleton = mapMaybe (recurseFilterSections f) pageSkeleton}

isCategoriesPara :: PageSkeleton -> Bool
isCategoriesPara (Para (Paragraph _ paraBody)) =
    all isCategoryLinks paraBody
  where
    isCategoryLinks :: ParaBody -> Bool
    isCategoryLinks (ParaText _) = False
    isCategoryLinks (ParaLink (Link (PageName name) _ _ _)) =
      "Category:" `T.isPrefixOf` name
isCategoriesPara _ = False

topLevelFilterPage :: (PageSkeleton -> Bool) -> Page  ->  Page
topLevelFilterPage f page =
    page { pageSkeleton = filter f $ pageSkeleton page }

dropPageIfShort :: Page -> Maybe Page
dropPageIfShort page@(Page {pageSkeleton=pageSkeleta})
  | length pageSkeleta > 3 = Just page
  | otherwise              = Nothing

transformFull :: Page -> Maybe Page
transformFull page =
    dropPageIfShort
    $ recurseFilterPage (not . isForbiddenHeading forbiddenStdHeadings)
    $ recurseFilterPage (not . isShortHeading)
    $ recurseFilterPage (not . isLongHeading)
    $ recurseFilterPage (not . isImage)
    $ recurseFilterPage (not . isCategoriesPara)
    $ topLevelFilterPage (not . isPara)
    page



transformCategoriesAndForbiddenSection :: Page -> Maybe Page
transformCategoriesAndForbiddenSection page =
    Just
    $ recurseFilterPage (not . isForbiddenHeading forbiddenStdHeadings)
    $ recurseFilterPage (not . isShortHeading)
    $ recurseFilterPage (not . isLongHeading)
    $ recurseFilterPage (not . isImage)
    $ recurseFilterPage (not . isCategoriesPara)
    page

---

main :: IO ()
main = do
    (inputFile, outputFile, transformMode) <- execParser $ info (helper <*> opts) (progDescDoc $ Just helpDescr)
    (prov, pages) <- readPagesFileWithProvenance inputFile
    writeCarFile outputFile prov $ mapMaybe transformMode pages

