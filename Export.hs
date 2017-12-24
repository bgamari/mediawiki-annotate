{-# LANGUAGE OverloadedStrings #-}

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
import CAR.CarExports as Exports
import CAR.AnnotationsFile as AnnsFile

options :: Parser (FilePath, [SiteId -> PageId], [Exporter])
options =
    (,,)
        <$> argument str (help "annotations file" <> metavar "FILE")
        <*> many (option (flip pageNameToId . PageName . T.pack <$> str)
                         (short 'p' <> long "page"
                          <> metavar "PAGE NAME" <> help "Export only this page")
              <|> option (const . packPageId <$> str)
                         (short 'P' <> long "page-id"
                          <> metavar "PAGE ID" <> help "Export only this page"))
        <*> some exporter
  where
    exporter :: Parser Exporter
    exporter = asum
        [ exportPages
          <$> option str (long "pages" <> metavar "OUTPUT" <> help "Export pages")

        , exportOutlines
          <$> option str (long "outlines" <> metavar "OUTPUT" <> help "Export outlines")

        , exportParagraphs
          <$> option str (long "paragraphs" <> metavar "OUTPUT" <> help "Export paragraphs")

        , exportParagraphAnnotations id
          <$> option str (long "para-hier-qrel" <> metavar "OUTPUT" <> help "Export hierarchical qrel for paragraphs")

        , exportParagraphAnnotations cutSectionPathArticle
          <$> option str (long "para-article-qrel" <> metavar "OUTPUT" <> help "Export hierarchical qrel for paragraphs")

        , exportParagraphAnnotations cutSectionPathTopLevel
          <$> option str (long "para-toplevel-qrel" <> metavar "OUTPUT" <> help "Export hierarchical qrel for paragraphs")

        , exportEntityAnnotations id
          <$> option str (long "entity-hier-qrel" <> metavar "OUTPUT" <> help "Export hierarchical qrel for entities")

        , exportEntityAnnotations cutSectionPathArticle
          <$> option str (long "entity-article-qrel" <> metavar "OUTPUT" <> help "Export hierarchical qrel for entities")

        , exportEntityAnnotations cutSectionPathTopLevel
          <$> option str (long "entity-toplevel-qrel" <> metavar "OUTPUT" <> help "Export hierarchical qrel for entities")
          
        , exportAllWithPrefix
          <$> option str (short 'o' <> long "output-prefix" <> metavar "PREFIX" <> help "Export all under prefix (backwards compatibility)")
        ]

type Exporter = Provenance -> [Page] -> IO ()

exportOutlines :: FilePath -> Exporter
exportOutlines outPath prov pagesToExport = do
    putStr "Writing outlines..."
    let skeletonFile = outPath 
    writeCarFile skeletonFile prov $ map toStubSkeleton pagesToExport
    putStrLn "done"

exportParagraphs :: FilePath -> Exporter
exportParagraphs outPath prov pagesToExport = do
    putStr "Writing paragraphs..."
    let paragraphsFile = outPath
    let sortIt = map snd . M.toAscList . foldMap (\para -> M.singleton (paraId para) para)
    writeCarFile paragraphsFile prov $ sortIt $ concatMap toParagraphs pagesToExport
    putStrLn "done"

exportParagraphAnnotations :: (SectionPath -> SectionPath) -> FilePath -> Exporter
exportParagraphAnnotations cutSectionPath outPath _prov pagesToExport = do
    putStr "Writing section relevance annotations..."
    let cutAnnotation (Annotation sectionPath paragId rel) =
          Annotation (cutSectionPath sectionPath) paragId rel
    writeFile outPath
          $ unlines
          $ map prettyAnnotation
          $ S.toList
          $ S.map cutAnnotation
          $ foldMap Exports.toAnnotations pagesToExport
    putStrLn "done"

cutSectionPathArticle, cutSectionPathTopLevel :: SectionPath -> SectionPath
cutSectionPathArticle (SectionPath pgId _headinglist)  =
    SectionPath pgId mempty
cutSectionPathTopLevel  (SectionPath pgId headinglist) =
    SectionPath pgId (take 1 headinglist)

exportEntityAnnotations :: (SectionPath -> SectionPath) -> FilePath -> Exporter
exportEntityAnnotations cutSectionPath outPath _prov pagesToExport = do
    putStr "Writing section relevance annotations..."
    let cutAnnotation (EntityAnnotation sectionPath entityId rel) =
          EntityAnnotation (cutSectionPath sectionPath) entityId rel
    writeFile outPath
          $ unlines
          $ map prettyEntityAnnotation
          $ S.toList
          $ S.map cutAnnotation
          $ foldMap Exports.toEntityAnnotations pagesToExport
    putStrLn "done"


exportPages :: FilePath -> Exporter
exportPages outPath prov pagesToExport = do
    putStr "Writing articles..."
    let articleFile = outPath
    writeCarFile articleFile prov pagesToExport
    putStrLn "done"



exportAllWithPrefix :: FilePath -> Exporter
exportAllWithPrefix outpath = do
    exportPages (outpath <.> "articles")
    exportOutlines (outpath <.> "outlines")
    exportParagraphs ( outpath <.> "paragraphs")
    exportParagraphAnnotations id (outpath <.> "hierarchical.qrels")
    exportParagraphAnnotations cutSectionPathArticle  (outpath <.> "article.qrels")
    exportParagraphAnnotations cutSectionPathTopLevel (outpath <.> "toplevel.qrels")
    exportEntityAnnotations id  (outpath <.> "hierarchical.entity.qrels")
    exportEntityAnnotations cutSectionPathArticle (outpath <.> "article.entity.qrels")
    exportEntityAnnotations cutSectionPathTopLevel  (outpath <.> "toplevel.entity.qrels") 


main :: IO ()
main = do
    (path, names, exporters) <- execParser $ info (helper <*> options) mempty
    anns <- openAnnotations path
    (prov, _) <- readPagesFileWithProvenance path
    let siteId = wikiSite prov

    forM_ exporters $ \exporter ->
        let pagesToExport
              | null names = pages anns
              | otherwise  = mapMaybe (`lookupPage` anns)
                           $ map ($ siteId) names
            {-# INLINE pagesToExport #-}
        in exporter prov pagesToExport
