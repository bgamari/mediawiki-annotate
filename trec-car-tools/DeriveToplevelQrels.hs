{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

import Data.Monoid
import qualified Data.HashMap.Strict as HM

import Options.Applicative

import CAR.Types
import CAR.ToolVersion
import CAR.CarExports as Exports
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

    let topLevelAnnotations :: HM.HashMap (SectionPath, ParagraphId) (Annotation GradedRelevance)
        topLevelAnnotations = HM.fromListWith maxAnnotation
                              [ ((topLevelSectionpath, paragraphId) , Annotation topLevelSectionpath paragraphId relevance)
                              | Annotation hierarchicalSectionpath paragraphId relevance <- hierarchicalAnnotations
                              , let topLevelSectionpath = cutTopLevel hierarchicalSectionpath
                              ]
                                where cutTopLevel (SectionPath {sectionPathPageId, sectionPathHeadings} ) =
                                          SectionPath sectionPathPageId $ take 1 sectionPathHeadings
                                      maxAnnotation  ann1@(Annotation _ _ relevance1) ann2@( Annotation _ _ relevance2) =
                                          if relevance1 > relevance2 then ann1
                                                                     else ann2

        topLevelAnnotations' = HM.elems topLevelAnnotations
    writeParagraphQRel outPath topLevelAnnotations'
