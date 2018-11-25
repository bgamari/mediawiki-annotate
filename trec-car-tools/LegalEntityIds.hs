{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid
import Data.List

import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.IO as TL

import Options.Applicative

import CAR.Types
import CAR.AnnotationsFile as CAR

-- TODO: This should get moved to trec-car-dump

options :: Parser (FilePath, FilePath)
options =
    (,)
      <$> argument str (help "annotations file" <> metavar "FILE" <> help "unprocessed all.cbor file")
      <*> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")

main :: IO ()
main = do
    (unprocessedPagesFile, outputFile) <- execParser $ info (helper <*> options) mempty
--     unprocessedPages <- openAnnotations unprocessedPagesFile
    pageBundle <- CAR.openPageBundle unprocessedPagesFile

    let legalPages = CAR.bundleAllPages pageBundle --CAR.pages unprocessedPages

        formatPageIdToName :: Page -> TB.Builder
        formatPageIdToName page =
            (TB.fromString $ unpackPageId $ pageId page)
            <> "\t"
            <> (TB.fromString $ unpackPageName $ pageName page)

    TL.writeFile outputFile $ TB.toLazyText
         $ mconcat
         $ intersperse "\n"
         $ fmap formatPageIdToName
         legalPages
