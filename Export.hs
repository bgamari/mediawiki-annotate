{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
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

options :: Parser (FilePath, FilePath, [SiteId -> PageId])
options =
    (,,)
        <$> argument str (help "annotations file" <> metavar "FILE")
        <*> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")
        <*> many (option (flip pageNameToId . PageName . T.pack <$> str)
                         (short 'p' <> long "page"
                          <> metavar "PAGE NAME" <> help "Export only this page")
              <|> option (const . packPageId <$> str)
                         (short 'P' <> long "page-id"
                          <> metavar "PAGE ID" <> help "Export only this page"))

exportOutlines :: FilePath -> Provenance -> [Page] -> IO ()
exportOutlines outPath prov pagesToExport = do
    putStr "Writing outlines..."
    let skeletonFile = outPath <.> "outlines"
    writeCarFile skeletonFile prov $ map toStubSkeleton pagesToExport
    putStrLn "done"

exportParagraphs :: FilePath -> Provenance -> [Page] -> IO ()
exportParagraphs outPath prov pagesToExport = do
    putStr "Writing paragraphs..."
    let paragraphsFile = outPath <.> "paragraphs"
    let sortIt = map snd . M.toAscList . foldMap (\para -> M.singleton (paraId para) para)
    writeCarFile paragraphsFile prov $ sortIt $ concatMap toParagraphs pagesToExport
    putStrLn "done"

exportParagraphAnnotations :: (SectionPath -> SectionPath) -> FilePath -> Provenance -> [Page] -> IO ()
exportParagraphAnnotations cutSectionPath outPath _prov pagesToExport = do
    putStr "Writing section relevance annotations..."
    let cutAnnotation (Annotation sectionPath paraId rel) =
          Annotation (cutSectionPath sectionPath) paraId rel
    writeFile outPath
          $ unlines
          $ map prettyAnnotation
          $ S.toList
          $ S.map cutAnnotation
          $ foldMap Exports.toAnnotations pagesToExport
    putStrLn "done"

exportEntityAnnotations :: (SectionPath -> SectionPath) -> FilePath -> Provenance -> [Page] -> IO ()
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


exportPages :: FilePath -> Provenance -> [Page] -> IO ()
exportPages outPath prov pagesToExport = do
    putStr "Writing articles..."
    let articleFile = outPath <.> "articles"
    writeCarFile articleFile prov pagesToExport
    putStrLn "done"

main :: IO ()
main = do
    (path, outPath, names) <- execParser $ info (helper <*> options) mempty
    anns <- openAnnotations path
    (prov, _) <- readPagesFileWithProvenance path
    let siteId = wikiSite prov
    let pagesToExport
          | null names = pages anns
          | otherwise  = mapMaybe (`lookupPage` anns)
                         $ map ($ siteId) names
        {-# INLINE pagesToExport #-}

    unless (null names) $ exportPages outPath prov pagesToExport
    exportOutlines outPath prov pagesToExport
    exportParagraphs outPath prov pagesToExport

    let cutSectionPathArticle (SectionPath pgId _headinglist)  =
            SectionPath pgId mempty
    let cutSectionPathTopLevel  (SectionPath pgId headinglist) =
            SectionPath pgId (take 1 headinglist)

    exportParagraphAnnotations id                     (outPath <.> "hierarchical.qrels")        prov pagesToExport
    exportParagraphAnnotations cutSectionPathArticle  (outPath <.> "article.qrels")             prov pagesToExport
    exportParagraphAnnotations cutSectionPathTopLevel (outPath <.> "toplevel.qrels")            prov pagesToExport
    exportEntityAnnotations    id                     (outPath <.> "hierarchical.entity.qrels") prov pagesToExport
    exportEntityAnnotations    cutSectionPathArticle  (outPath <.> "article.entity.qrels")      prov pagesToExport
    exportEntityAnnotations    cutSectionPathTopLevel (outPath <.> "toplevel.entity.qrels")     prov pagesToExport

    return ()
