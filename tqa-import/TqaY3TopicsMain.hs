{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Hashable
import Data.Semigroup ((<>))
import Data.Aeson as Aeson
import Data.Foldable
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL
import Options.Applicative
import qualified Data.Map.Strict as M
import Data.Maybe


import CAR.Types
import TQA

options :: Parser (FilePath, FilePath)
options =
    (,) <$> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output CAR pages file")
        <*> argument str (metavar "FILE" <> help "Input TQA JSON file")

main :: IO ()
main = do
    (outputPath, inputPath) <- execParser $ info (helper <*> options) mempty
    lessons <- either error id . Aeson.eitherDecode <$> BSL.readFile inputPath
    writeCarFile outputPath prov $ map lessonToPage lessons

