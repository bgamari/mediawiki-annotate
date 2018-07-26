{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.Monoid

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List
import System.FilePath

import Options.Applicative

import CAR.Types
import CAR.ToolVersion
import CAR.CarExports as Exports
import CAR.AnnotationsFile as AnnsFile
import CAR.QRelFile


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

        , exportParagraphAnnotations id id
          <$> option str (long "para-hier-qrel" <> metavar "OUTPUT" <> help "Export hierarchical qrel for paragraphs")

        , exportParagraphAnnotations id resolveSubsumption
          <$> option str (long "para-tree-qrel" <> metavar "OUTPUT" <> help "Export tree qrel for paragraphs")

        , exportParagraphAnnotations cutSectionPathArticle id
          <$> option str (long "para-article-qrel" <> metavar "OUTPUT" <> help "Export hierarchical qrel for paragraphs")

        , exportParagraphAnnotations cutSectionPathTopLevel id
          <$> option str (long "para-toplevel-qrel" <> metavar "OUTPUT" <> help "Export hierarchical qrel for paragraphs")

        , exportEntityAnnotations id id
          <$> option str (long "entity-hier-qrel" <> metavar "OUTPUT" <> help "Export hierarchical qrel for entities")

        , exportEntityAnnotations id resolveEntitySubsumption
          <$> option str (long "entity-tree-qrel" <> metavar "OUTPUT" <> help "Export tree qrel for entities")

        , exportEntityAnnotations cutSectionPathArticle id
          <$> option str (long "entity-article-qrel" <> metavar "OUTPUT" <> help "Export hierarchical qrel for entities")

        , exportEntityAnnotations cutSectionPathTopLevel id
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

exportParagraphAnnotations :: (SectionPath -> SectionPath) -> ([Annotation IsRelevant] -> [Annotation IsRelevant]) -> FilePath -> Exporter
exportParagraphAnnotations cutSectionPath transform outPath _prov pagesToExport = do
    putStr "Writing section relevance annotations..."
    let cutAnnotation (Annotation sectionPath paragId rel) =
          Annotation (cutSectionPath sectionPath) paragId rel
    writeParagraphQRel outPath
          $ transform
          $ S.toList
          $ S.map cutAnnotation
          $ foldMap Exports.toAnnotations pagesToExport
    putStrLn "done"


resolveSubsumption :: [Annotation IsRelevant] -> [Annotation IsRelevant]
resolveSubsumption annotations =
        [ (Annotation (SectionPath {sectionPathPageId=pageid, sectionPathHeadings=headingPrefix}) paraid rel)
        | (Annotation (SectionPath {sectionPathPageId=pageid, sectionPathHeadings=headings}) paraid rel) <- annotations
        , headingPrefix <- heads headings
        ]
  where heads xs = map reverse $ tails $ reverse xs

resolveEntitySubsumption ::  [EntityAnnotation IsRelevant] -> [EntityAnnotation IsRelevant]
resolveEntitySubsumption annotations =
        [ (EntityAnnotation (SectionPath {sectionPathPageId=pageid, sectionPathHeadings=headingPrefix}) paraid rel)
        | (EntityAnnotation (SectionPath {sectionPathPageId=pageid, sectionPathHeadings=headings}) paraid rel) <- annotations
        , headingPrefix <- heads headings
        ]
  where heads xs = map reverse $ tails $ reverse xs

cutSectionPathArticle, cutSectionPathTopLevel :: SectionPath -> SectionPath
cutSectionPathArticle (SectionPath pgId _headinglist)  =
    SectionPath pgId mempty
cutSectionPathTopLevel  (SectionPath pgId headinglist) =
    SectionPath pgId (take 1 headinglist)

exportEntityAnnotations :: (SectionPath -> SectionPath) -> ([EntityAnnotation IsRelevant] -> [EntityAnnotation IsRelevant]) -> FilePath -> Exporter
exportEntityAnnotations cutSectionPath transform outPath _prov pagesToExport = do
    putStr "Writing section relevance annotations..."
    let cutAnnotation (EntityAnnotation sectionPath entityId rel) =
          EntityAnnotation (cutSectionPath sectionPath) entityId rel
    writeEntityQRel outPath
          $ transform
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
exportAllWithPrefix outpath prov pages= do
    exportPages (outpath <> "-articles.cbor") prov pages
    exportOutlines (outpath <> "-outlines.cbor") prov pages
    exportParagraphs ( outpath <> "-paragraphs.cbor") prov pages
    exportParagraphAnnotations id id (outpath <.> "hierarchical.qrels") prov pages
    exportParagraphAnnotations id resolveSubsumption (outpath <.> "tree.qrels") prov pages
    exportParagraphAnnotations cutSectionPathArticle id  (outpath <.> "article.qrels") prov pages
    exportParagraphAnnotations cutSectionPathTopLevel id (outpath <.> "toplevel.qrels") prov pages
    exportEntityAnnotations id id  (outpath <.> "hierarchical.entity.qrels") prov pages
    exportEntityAnnotations id resolveEntitySubsumption (outpath <.> "tree.entity.qrels") prov pages
    exportEntityAnnotations cutSectionPathArticle id (outpath <.> "article.entity.qrels") prov pages
    exportEntityAnnotations cutSectionPathTopLevel id (outpath <.> "toplevel.entity.qrels") prov pages


main :: IO ()
main = do
    (path, names, exporters) <- execParser' 2 (helper <*> options) mempty
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
