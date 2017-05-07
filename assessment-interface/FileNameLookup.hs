{-# LANGUAGE RecordWildCards #-}

module FileNameLookup where


import Options.Applicative

import Data.Monoid
import Data.Traversable
import Data.List
import Data.Maybe
import Data.Foldable
import System.FilePath
import System.Directory
import Network.URI


import System.Random
import System.Random.Shuffle
import Control.Monad.Random

import qualified Data.Text as T

import CAR.Types
import CAR.CarExports
import qualified CAR.TocFile as TocFile
import qualified SimplIR.Format.TrecRunFile as TrecRun


data FileNameLookup = FileNameLookup { outlinePathname :: Stub -> FilePath
                             , outlineURL :: Stub -> FilePath
                             , passageViewPathname :: SectionPath -> Maybe FilePath
                             , viewURL :: FilePath -> String
                             , maybePassageViewUrl :: SectionPath -> Maybe FilePath
                             }

fileNameLookupFactory :: (SectionPath -> Bool) -> FileNameLookup
fileNameLookupFactory existResultsForSectionpath  = FileNameLookup {..}
  where
    outlinePathname :: Stub -> FilePath
    outlinePathname (Stub _ pageId _) =
       (unpackPageId pageId) </> "index" <.> "html"

    outlineURL :: Stub -> FilePath
    outlineURL stub =
        escapeURIString isUnreserved (outlinePathname stub)



    passageViewPathname :: SectionPath -> Maybe FilePath
    passageViewPathname sectionPath@(SectionPath page headings)
        | existResultsForSectionpath sectionPath = Just (unpackPageId page </> sectionfilename <.> "html")
        | otherwise = Nothing
      where
        sectionfilename =
          case headings of
            [] -> "index-article"
            _ ->  intercalate "-" $ map (map replaceChars . unpackHeadingId) headings
          where
            replaceChars '/' = '-'
            replaceChars c   = c

    viewURL :: FilePath -> String
    viewURL inputFile =
        escapeURIString isUnreserved inputFile


    maybePassageViewUrl sectionPath = fmap viewURL (passageViewPathname sectionPath)