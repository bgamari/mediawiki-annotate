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
import Data.Hashable
import Codec.Serialise
import Data.Maybe

import Options.Applicative
import LookupWrapper
import CAR.ToolVersion
import CAR.TocFile as Toc
import CAR.Types
import CAR.Types.CborList
import CAR.Types.Files

queryHelpDesc :: PP.Doc
queryHelpDesc = "Create PageDoc corpus and toc"


buildPageDocToc :: (Eq i, Hashable i, Serialise i) => FilePath -> IO (IndexedCborPath i (AbstractDoc i))
buildPageDocToc cborPath =
    Toc.createIndex pageDocId cborPath
--     void $ Toc.createIndex pageDocId cborpath

convertPageToPageDocMode =
    go <$> argument str (metavar "CBOR" <> help "page cbor file")
       <*> option str (long "output" <> short 'o' <> help "output index path")
       <*> flag Nothing (Just NeighborsFromInlinks) (long "inlinks" <> help "Include entities linking to this page as neighbors")
       <*> flag Nothing (Just NeighborsFromOutlinks) (long "outlinks" <> help "Include entities linked to by this page as neighbors")
       <*> flag Nothing (Just NeighborsFromBidilinks) (long "bidilinks" <> help "Include entities only when in a bidirectional link (linked to and from this page) as neighbors")
  where
    go inputPath outputPath neighborsFromInlinks neighborsFromOutlinks neighborsFromBidilinks = do
        pages <- readPagesFile inputPath
        exportPageDocsFromPages pages outputPath whichNeighbors
      where     whichNeighbors :: [WhichNeighbors]
                whichNeighbors =
                    -- bidi trumps the other two. If bidi is Nothing, then use the concat of in/out
                   if neighborsFromBidilinks == Just NeighborsFromBidilinks then
                        [NeighborsFromBidilinks]
                    else catMaybes [neighborsFromInlinks, neighborsFromOutlinks]



    exportPageDocsFromPages:: [Page] -> FilePath -> [WhichNeighbors] -> IO ()
    exportPageDocsFromPages pagesToExport outPath whichNeighbors = do
        putStr "Writing pagedocs..."
        let edgeDocFile = outPath
    --     writeCarFile edgeDocFile $ map pageToEdgeDocs pagesToExport
        writeCborList outPath (Just (whichNeighbors)) $ foldMap (pageToPageDocs whichNeighbors) pagesToExport
        putStrLn "done"




buildTocMode =
    go <$> argument str (metavar "CBOR" <> help "PageDoc cbor file")
  where
    go inputPath = do
        void $ buildPageDocToc @PageId inputPath

lookupMode =
    go <$> argument (Toc.IndexedCborPath <$> str) (metavar "CBOR" <> help "PageDoc cbor file")
       <*> argument (packPageId <$> str) (metavar "PageId" <> help "page id of pagedoc to look up.")
  where
    go :: Toc.IndexedCborPath PageId PageDoc -> PageId -> IO ()
    go inputPath pageId = do
        toc <- Toc.open inputPath
        let pageDoc :: Maybe PageDoc
            pageDoc = pageId `Toc.lookup` toc
        putStrLn $ show pageDoc

modes = subparser
    $ command "convert-page" (info (helper <*> convertPageToPageDocMode) (progDesc "Convert pages to pagedocs" <> fullDesc))
   <> command "toc" (info (helper <*> buildTocMode) (progDesc "build toc filed for pagedocs" <> fullDesc))
   <> command "lookup" (info (helper <*> lookupMode) (progDesc "get pagedoc by pageId" <> fullDesc))

main :: IO ()
main = do
    mode <- execParser' 2 (helper <*> modes)  fullDesc
    mode
