{-# LANGUAGE ScopedTypeVariables #-}

import Data.Monoid

import qualified Data.HashSet as HS
import Options.Applicative

import CAR.Types
import CAR.TocFile  as TocFile
import CAR.FilterDuplicates.Utils
{-# ANN module ("HLint: ignore Redundant $"::String) #-}


modes = subparser
    $ command "filter"  (info (helper <*> filterparasModes) mempty)
   <> command "test" (info (helper <*> testMode) mempty)

main :: IO ()
main = do
    mode <- execParser $ info (helper <*> modes) mempty
    mode


filterparasModes :: Parser (IO ())
filterparasModes = go
    <$> option str  (long "duplicates" <> metavar "FILE" <> help "True duplicate table for comparison")
    <*> option str (long "paragraphs" <> metavar "CBOR" <> help "paragraph CBOR")
    <*> optional (option auto (short 'n' <> long "take" <> metavar "INT" <> help "number of edges to include"))
    <*> option str (short 'o' <> long "output" <> metavar "FILE" <> help "file to write filtered paragraphs to")
  where
    go :: FilePath -> FilePath -> Maybe Int -> FilePath -> IO ()
    go duplicatesFile paragraphsFile nTake outFile = do
        (prov, paras) <- readParagraphsFileWithProvenance paragraphsFile

        edges <- parseDuplicates <$> readFile duplicatesFile
        let duplicateParagraphIds = HS.fromList
                                  $ foldMap (\(a,b) -> [a, b])
                                  $ maybe id take nTake
                                  $ edges

            filteredParagraphs = filter inDuplicates paras
                where inDuplicates para = (paraId para) `HS.member` duplicateParagraphIds
        writeCarFile outFile prov filteredParagraphs



testMode :: Parser (IO ())
testMode = go
    <$> option str (short 'T' <> long "true-duplicates" <> metavar "FILE" <> help "True duplicate table for comparison")
    <*> option str (short 't' <> long "test-duplicates" <> metavar "FILE" <> help "Test duplicate table for comparison")
    <*> optional (option (IndexedCborPath <$> str)
                         (short 'p' <> long "paragraphs" <> metavar "FILE" <> help "Paragraphs file for full text lookup"))
  where
    go :: FilePath -> FilePath -> Maybe (IndexedCborPath ParagraphId Paragraph) -> IO ()
    go trueDuplicatesFile testDuplicatesFile paragraphsFile = do
        trueEdges <- parseDuplicates <$> readFile trueDuplicatesFile
        testEdges <- parseDuplicates <$> readFile testDuplicatesFile
        paragraphs <- traverse TocFile.open paragraphsFile

        let trueEdgeSet = HS.fromList $ [ HS.fromList [a,b] | (a,b) <- trueEdges]
            testEdgeSet = HS.fromList $ [ HS.fromList [a,b] | (a,b) <- testEdges]

            missingEdges = trueEdgeSet `HS.difference` testEdgeSet

        putStrLn $ "Number of missing edges: " <> show (HS.size missingEdges)

        putStrLn $ "Sample: "
                <> unlines
                 ( fmap (prettyEdge paragraphs)
                 $ take 5
                 $ HS.toList missingEdges
                 )
      where prettyEdge :: Maybe (IndexedCbor ParagraphId Paragraph) -> HS.HashSet ParagraphId -> String
            prettyEdge paragraphs edge
              | Just paras <- paragraphs =
                 let Just pa = TocFile.lookup a paras
                     Just pb = TocFile.lookup b paras
                 in unlines $ map (prettyParagraph anchorOnly) [pa,pb]
              | otherwise = show a <> " -- " <> show b
              where [a, b] =  HS.toList edge
