{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ApplicativeDo #-}

import Data.Semigroup hiding (option)
import Options.Applicative

import CAR.Utils
import CAR.Types
import CAR.Utils.Redirects
import CAR.FillMetadata

data Stage = StageResolveRedirect
           | StageResolveDisambiguationAndInlinks
           | StageCategoryTags


data Opts = Opts { inputPath :: FilePath
                 , outputPath :: FilePath
                 , stage :: Stage
                 }


opts :: Parser Opts
opts = do
    inputPath <- option str (short 'i' <> long "input" <> metavar "INFILE" <> help "Input CBOR pages file" )
    outputPath <- option str (short 'o' <> long "output" <> metavar "OUTFILE" <> help "Output CBOR pages file ")
    stage <- redirect <|> disambigInlinks  <|> categoryTags
    return Opts {..}
  where
    redirect = flag' StageResolveRedirect (short 'r' <> long "redirect" <> help "Resolve redirect stage")
    disambigInlinks = flag' StageResolveDisambiguationAndInlinks (short 'd' <> long "disambiguation" <> help "Collect disambiguation names and inlinks")
    categoryTags = flag' StageCategoryTags (short 'c' <> long "category" <> help "Load category tags into meta data")

main :: IO ()
main = do
    Opts{..} <- execParser $ info (helper <*> opts) $ progDescDoc (Just "Fill in derived page metadata. ")

    (prov, pages') <-
      case stage of
          StageResolveRedirect                 -> stageResolveRedirect inputPath
          StageResolveDisambiguationAndInlinks -> stageResolveDisambiguationAndInlinks inputPath
          StageCategoryTags                    -> stageResolveDisambiguationAndInlinks inputPath
    writeCarFile outputPath prov pages'


