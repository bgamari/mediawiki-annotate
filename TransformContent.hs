{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid hiding (All, Any)
import Data.Void
import System.IO
import Options.Applicative
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as BSB
import qualified Text.Trifecta as Tri
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Text.PrettyPrint.ANSI.Leijen ((<+>), (<$$>))
import Data.Maybe

import CAR.Types


helpDescr :: PP.Doc
helpDescr =
    "Predicate options:" <$$> PP.indent 4 opts
  where
    cmd a b = PP.nest 8 (a <$$> b)
    opts = PP.vsep
      [
        cmd "need doc"                           "here"
      ]

opts :: Parser (FilePath, FilePath)
opts =
    (,)
    <$> argument str (help "input file" <> metavar "ANNOTATIONS FILE")
    <*> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")
    -- <*> argument predicate (metavar "PRED" <> help "Predicate")

forbiddenHeadings = HS.fromList ["see also", "references", "external links", "notes",
    "bibliography", "gallery", "publications", "further reading", "track listing", "sources",
    "cast", "discography", "awards"]

isLead :: PageSkeleton -> Bool
isLead (Para _) = True
isLead (Section _ _ _) = False

isForbiddenSection :: PageSkeleton -> Bool
isForbiddenSection (Section heading _ _) =
    ( T.toCaseFold (getSectionHeading heading) `HS.member` forbiddenHeadings)
isForbiddenSection x = False

recurseDropForbiddenSections :: PageSkeleton -> Maybe PageSkeleton
recurseDropForbiddenSections (Section heading id children)
    | null children' = Nothing
    | otherwise =  Just (Section heading id children')
    where children' = mapMaybe recurseDropForbiddenSections $ filter isForbiddenSection children
recurseDropForbiddenSections x = Just x


transformContent :: Page -> Maybe Page
transformContent (Page pageName pageId pageSkeleta)
  | ((length pageSkeleta')  > 3) =
      Just (Page pageName pageId pageSkeleta')
  | otherwise = Nothing
  where
    pageSkeleta'=
          mapMaybe recurseDropForbiddenSections
        $ filter (\skel -> not (isForbiddenSection skel) &&  not (isLead skel)) pageSkeleta



main :: IO ()
main = do
    (inputFile, outputFile) <- execParser $ info (helper <*> opts) (progDescDoc $ Just helpDescr)
    pages <- decodeCborList <$> BSL.readFile inputFile
    withFile outputFile WriteMode $ \h ->
        BSB.hPutBuilder h $ encodeCborList $ map transformContent $ pages
