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


data Opts = Opts { inputPath :: FilePath
                 , outputPath :: FilePath
                 , stage :: Stage
                 }


opts :: Parser Opts
opts = do
    inputPath <- option str (short 'i' <> long "input" <> metavar "INFILE" <> help "Input CBOR pages file" )
    outputPath <- option str (short 'o' <> long "output" <> metavar "OUTFILE" <> help "Output CBOR pages file ")
    stage <- flag StageResolveRedirect StageResolveDisambiguationAndInlinks (short 'r' <> long "redirect" <> help "If set, execute redirect resolution step")
--     stage <- redirect <|> disambigInlinks <|> hello
--     stage <- option (str >>= readStage) (short 'm' <> long "mode" <> metavar "STAGE" <> help "Stage")
    return Opts {..}
  where
--     readStage "hello" = return StageHello
--     readStage "redirect" = return StageResolveRedirect
--     readStage other = fail $ "Unknown stage "++other

--     redirect = flag' StageResolveRedirect (long "redirect")
--     disambigInlinks = flag' StageResolveDisambiguationAndInlinks (long "disambig")
--     hello = flag' StageHello (long "hello")

main :: IO ()
main = do
    Opts{..} <- execParser $ info (helper <*> opts) $ progDescDoc (Just "Fill in derived page metadata. ")

    case stage of
      StageResolveRedirect ->  do
        acc <- unionsWith (<>) . fmap buildRedirectMap <$> readPagesFile inputPath
        redirectResolver <- resolveRedirects <$> readPagesFile inputPath

        (prov, pages) <- readPagesFileWithProvenance inputPath
        let pages' = map ((fixLinks redirectResolver) . (fillRedirectMetadata acc)) pages
        writeCarFile outputPath prov pages'
      StageResolveDisambiguationAndInlinks ->  do
        acc <- unionsWith (<>) . fmap buildMap <$> readPagesFile inputPath
        (prov, pages) <- readPagesFileWithProvenance inputPath
        let pages' = map (fillMetadata acc) pages
        writeCarFile outputPath prov pages'

