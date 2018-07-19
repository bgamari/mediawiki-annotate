{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid hiding (All, Any)
import Data.Void
import Control.Monad (void)
import Options.Applicative
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import qualified Data.Binary.Serialise.CBOR as CBOR

import CAR.ToolVersion
import CAR.Types

helpDescr :: PP.Doc
helpDescr =
    "Remove duplicate page entries from a pages file"

opts :: Parser (FilePath, FilePath)
opts =
    (,)
    <$> argument str (help "input file" <> metavar "ANNOTATIONS FILE")
    <*> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")


main :: IO ()
main = do
    (inputFile, outputFile) <-
        execParser' 1 (helper <*> opts) (progDescDoc $ Just helpDescr)
    (prov, pages) <- readPagesFileWithProvenance inputFile

    let pages' = go mempty pages
          where go seen (p:ps)
                  | (pageId p) `S.member` seen = go seen ps
                  | otherwise  = p : go ((pageId p) `S.insert` seen) ps
                go seen [] = []


    writeCarFile outputFile prov pages'

