{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}

import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Data.Monoid
import Control.Monad

import Options.Applicative
import EdgeDocCorpus
import CAR.ToolVersion
import CAR.TocFile as Toc
import CAR.Types
import CAR.Types.CborList
import CAR.Types.Files



queryHelpDesc :: PP.Doc
queryHelpDesc = "Create EdgeDoc corpus and toc"




exportEdgeDocs :: [Page] -> FilePath -> IO ()
exportEdgeDocs pagesToExport outPath = do
    putStr "Writing edgedocs..."
    let edgeDocFile = outPath
--     writeCarFile edgeDocFile $ map pageToEdgeDocs pagesToExport
    writeCborList outPath (Just (0::Int)) $ foldMap pageToEdgeDocs pagesToExport
    putStrLn "done"

buildEdgeDocToc :: FilePath -> IO ()
buildEdgeDocToc cborPath =
    void $ Toc.createIndex edgeDocParagraphId cborPath


convertMode =
    go <$> argument str (metavar "CBOR" <> help "pages cbor file")
       <*> option str (long "output" <> short 'o' <> help "output index path")
  where
    go inputPath outputPath = do
        pages <- readPagesFile inputPath
        exportEdgeDocs pages outputPath

buildTocMode =
    go <$> argument str (metavar "CBOR" <> help "EdgeDoc cbor file")
  where
    go inputPath = do
        buildEdgeDocToc inputPath

lookupMode =
    go <$> argument (Toc.IndexedCborPath <$> str) (metavar "CBOR" <> help "EdgeDoc cbor file")
       <*> argument (packParagraphId <$> str) (metavar "ParagraphId" <> help "paragraph id of edge doc to look up.")
  where
    go :: Toc.IndexedCborPath ParagraphId EdgeDoc -> ParagraphId -> IO ()
    go inputPath paragraphId = do
        toc <- Toc.open inputPath
        let edgeDoc :: Maybe EdgeDoc
            edgeDoc = paragraphId `Toc.lookup` toc
        putStrLn $ show edgeDoc

modes = subparser
    $ command "convert"  (info (helper <*> convertMode) (progDesc "Convert pages to edgedocs" <> fullDesc))
   <> command "toc" (info (helper <*> buildTocMode) (progDesc "build toc filed for edgedocs" <> fullDesc))
   <> command "lookup" (info (helper <*> lookupMode) (progDesc "get edgedoc by paragraphId" <> fullDesc))

main :: IO ()
main = do
    mode <- execParser' 1 (helper <*> modes)  fullDesc
    mode
