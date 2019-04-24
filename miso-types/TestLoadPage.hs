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


options :: Parser (FilePath)
options =
--     ()
--       <$>
      option str (short 'p' <> long "page" <> metavar "PageFILE" <> help "Page definition file (JSON file)")

main :: IO ()
main = do
    (pageFile) <- execParser $ info options mempty

    page <- either error id . Aeson.eitherDecode <$> BSL.readFile pageFile
         :: IO AssessmentPage

    Data.ByteString.Lazy.Char8.putStrLn $ AesonPretty.encodePretty page