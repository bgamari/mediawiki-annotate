-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields#-}

module Main where

import Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import Data.Text.Encoding

import Options.Applicative
import Control.Monad
import GHC.Generics
import Control.Exception
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe
import Data.Hashable

import Options.Applicative
import qualified Data.Text as T

import CAR.Types
import Types
import qualified OldTypes as OldTypes

opts :: Parser (IO ())
opts = subparser
    $ cmd "page" loadPage'
    <> cmd "old-assessments" loadOldAssessments'
    <> cmd "assessments" loadAssessments'
    <> cmd "merge-pages" mergePages'
  where cmd name action = command name (info (helper <*> action) fullDesc)
        loadPage' = loadPage
                  <$> option str (short 'p' <> long "page" <> metavar "PageFILE" <> help "Page definition file (JSON file)")
        loadOldAssessments' = loadOldAssessments
                  <$> option str (short 'a' <> long "assessments" <> metavar "AssessFILE (old)" <> help "Assessment file (JSON file)")
        loadAssessments' = loadAssessments
                  <$> option str (short 'a' <> long "assessments" <> metavar "AssessFILE (new)" <> help "Assessment file (JSON file)")
        mergePages' = mergePages
                  <$> some (argument str (metavar "PageFILE" <> help "Page definition file (JSON file)"))
                  <*> option str (short 'o' <> long "output" <> metavar "JSON" <> help "Json file to write the merged paged to")

loadPage pageFile = do
    page <- either error id . Aeson.eitherDecode <$> BSL.readFile pageFile
         :: IO AssessmentPage

    Data.ByteString.Lazy.Char8.putStrLn $ AesonPretty.encodePretty page


loadOldAssessments assessmentFile = do
    page <- either error id . Aeson.eitherDecode <$> BSL.readFile assessmentFile
         :: IO OldTypes.SavedAssessments

--     print "facetState (OldType)"
--     print $ OldTypes.facetState $ OldTypes.savedData page

    Data.ByteString.Lazy.Char8.putStrLn $ AesonPretty.encodePretty page

loadAssessments assessmentFile = do
    page <- either error id . Aeson.eitherDecode <$> BSL.readFile assessmentFile
         :: IO Types.SavedAssessments

--     print "facetState (NewType)"
--     print $ Types.facetState $ Types.savedData page

    Data.ByteString.Lazy.Char8.putStrLn $ AesonPretty.encodePretty page

mergePages :: [FilePath] -> FilePath -> IO ()
mergePages pageFiles outFile = do
    pages <- mapM load pageFiles
             :: IO [AssessmentPage]
    let out = SubmissionRun pages
    BSL.writeFile outFile $ Aeson.encode out

  where load :: FilePath -> IO AssessmentPage
        load pageFile = do
            page <- either error id . Aeson.eitherDecode <$> BSL.readFile pageFile
                 :: IO AssessmentPage
            return page




main :: IO ()
main = join $ execParser' (helper <*> opts) mempty

execParser' :: Parser a -> InfoMod a -> IO a
execParser' parser pinfo =
    execParser $ info (parser) pinfo
