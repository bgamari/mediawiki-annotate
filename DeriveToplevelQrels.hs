{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}


import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.Monoid

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.FilePath

import Options.Applicative

import CAR.Types
import CAR.ToolVersion
import CAR.CarExports as Exports
import CAR.AnnotationsFile as AnnsFile
import CAR.QRelFile



opts :: Parser (FilePath, FilePath)
opts =
    (,)
        <$> argument str (help "qrel file" <> metavar "FILE" <> help "hierarchical qrel file")
        <*> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Top-level qrel file")


main :: IO ()
main = do
    (hierarchicalQrels, outPath) <- execParser' 1 (helper <*> opts) $ progDescDoc (Just "Derive top level qrels from hierarchical qrels.")
    hierarchicalAnnotations <- readParagraphQRel hierarchicalQrels

    let topLevelAnnotations :: [Annotation GradedRelevance]
        topLevelAnnotations = [ Annotation topLevelSectionpath paragraphId relevance
                              | Annotation hierarchicalSectionpath paragraphId relevance <- hierarchicalAnnotations
                              , let topLevelSectionpath = cutTopLevel hierarchicalSectionpath
                              ]
                                where cutTopLevel (SectionPath {sectionPathPageId, sectionPathHeadings} ) =
                                          SectionPath sectionPathPageId $ take 1 sectionPathHeadings

    writeParagraphQRel outPath topLevelAnnotations