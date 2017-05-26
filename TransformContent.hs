{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

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

opts :: Parser (FilePath, FilePath)
opts =
    (,)
    <$> argument str (help "input file" <> metavar "ANNOTATIONS FILE")
    <*> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")
    -- <*> argument predicate (metavar "PRED" <> help "Predicate")

forbiddenHeadings :: HS.HashSet T.Text
forbiddenHeadings = HS.fromList ["see also", "references", "external links", "notes",
    "bibliography", "gallery", "publications", "further reading", "track listing", "sources",
    "cast", "discography", "awards", "other"]

isLead :: PageSkeleton -> Bool
isLead (Para{})    = True
isLead (Section{}) = False
isLead (Image{})   = False

isForbiddenSkeleton :: PageSkeleton -> Bool
isForbiddenSkeleton (Section heading _ _) =
    ( T.toCaseFold (getSectionHeading heading) `HS.member` forbiddenHeadings )  ||  -- excluded
    T.length (T.filter isAlpha (getSectionHeading heading)) < 3 ||                      -- not enough letters
    T.length (getSectionHeading heading) > 100                                          -- too long, probably parse error
isForbiddenSkeleton (Image _ _) = True
isForbiddenSkeleton _ = False

recurseDropForbiddenSections :: PageSkeleton -> Maybe PageSkeleton
recurseDropForbiddenSections (Section heading sectionId children)
    | null children' = Nothing
    | otherwise =  Just (Section heading sectionId children')
  where children' = mapMaybe recurseDropForbiddenSections $ filter (not . isForbiddenSkeleton) children
recurseDropForbiddenSections x = Just x


transformContent :: Page -> Maybe Page
transformContent (Page {pageName, pageId, pageSkeleton=pageSkeleta})
  | length pageSkeleta' > 3 =
      Just (Page pageName pageId pageSkeleta')
  | otherwise = Nothing
  where
    pageSkeleta'=
         mapMaybe recurseDropForbiddenSections
         $ filter (\skel -> not (isForbiddenSkeleton skel) &&  not (isLead skel)) pageSkeleta



main :: IO ()
main = do
    (inputFile, outputFile) <- execParser $ info (helper <*> opts) (progDescDoc $ Just helpDescr)
    pages <- decodeCborList <$> BSL.readFile inputFile
    withFile outputFile WriteMode $ \h ->
        BSB.hPutBuilder h $ encodeCborList $ mapMaybe transformContent $ pages
