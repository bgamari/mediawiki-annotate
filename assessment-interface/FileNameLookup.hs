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
                             , entityViewPathname :: SectionPath -> Maybe FilePath
                             , viewURL :: FilePath -> String
                             , maybePassageViewUrl :: SectionPath -> Maybe FilePath
                             , maybeEntityViewUrl :: SectionPath -> Maybe FilePath
                             }

fileNameLookupFactory :: (SectionPath -> Bool) -> (SectionPath -> Bool) -> FileNameLookup
fileNameLookupFactory existResultsForSectionpath  existEntityResultsForSectionpath = FileNameLookup {..}
  where
    outlinePathname :: Stub -> FilePath
    outlinePathname (Stub _ pageId _) =
       (unpackPageId pageId) </> "index" <.> "html"

    outlineURL :: Stub -> FilePath
    outlineURL stub =
      outlinePathname stub  -- do not percent encode filename, we leave this to the browser

    passageViewPathname :: SectionPath -> Maybe FilePath
    passageViewPathname sectionpath = if existResultsForSectionpath sectionpath
                                      then Just (viewPathname "psg" sectionpath)
                                      else Nothing

    entityViewPathname :: SectionPath -> Maybe FilePath
    entityViewPathname sectionpath = if existEntityResultsForSectionpath sectionpath
                                          then Just (viewPathname "entity" sectionpath)
                                          else Nothing


    viewPathname ::  String -> SectionPath -> FilePath
    viewPathname suffix sectionPath@(SectionPath page headings) =
        unpackPageId page </> sectionfilename <.> suffix <.> "html"
        -- do not percent encode filename, we leave this to the browser
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
           inputFile

    maybePassageViewUrl sectionPath = fmap viewURL (passageViewPathname sectionPath)
    maybeEntityViewUrl sectionPath = fmap viewURL (entityViewPathname sectionPath)