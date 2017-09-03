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
import CAR.Utils
import CAR.CarExports as Exports
import CAR.AnnotationsFile as AnnsFile

options :: Parser (FilePath, FilePath, FilePath, [SiteId -> PageId])
options =
    (,,,) <$> argument str (help "annotations file" <> metavar "FILE")
        <*> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")
        <*> option str (long "unproc" <> metavar "FILE" <> help "unprocessed all.cbor file")
        <*> many (option (flip pageNameToId . PageName . T.pack <$> str)
                         (short 'p' <> long "page"
                          <> metavar "PAGE NAME" <> help "Export only this page")
              <|> option (const . packPageId <$> str)
                         (short 'P' <> long "page-id"
                          <> metavar "PAGE ID" <> help "Export only this page"))


main :: IO ()
main = do
    (path, outpath, unprocessedPagesFile, names) <- execParser $ info (helper <*> options) mempty
    anns <- openAnnotations path
    (prov, _) <- readPagesFileWithProvenance path
    let siteId = wikiSite prov
    unprocessedPages <- openAnnotations unprocessedPagesFile
    let pagesToExport
          | null names = pages anns
          | otherwise  = mapMaybe (`lookupPage` anns)
                         $ map ($ siteId) names
        {-# INLINE pagesToExport #-}

    when (not $ null names) $ do
        putStr "Writing articles..."
        let articleFile = outpath <.> "articles"
        writeCarFile articleFile prov pagesToExport
        putStrLn "done"

    putStr "Writing outlines..."
    let skeletonFile = outpath <.> "outlines"
    writeCarFile skeletonFile prov $ map toStubSkeleton pagesToExport
    putStrLn "done"

    putStr "Writing paragraphs..."
    let paragraphsFile = outpath <.> "paragraphs"
    let sortIt = map snd . M.toAscList . foldMap (\para -> M.singleton (paraId para) para)
    writeCarFile paragraphsFile prov $ sortIt $ concatMap toParagraphs pagesToExport
    putStrLn "done"



    -- paragraph annotations
    let writeAnnotations ::  FilePath ->  (SectionPath -> SectionPath) -> IO ()
        writeAnnotations relsFile cutSectionPath = do
            putStr "Writing section relevance annotations..."
            let cutAnnotation (Annotation sectionPath paraId rel) =
                  Annotation (cutSectionPath sectionPath) paraId rel
            writeFile relsFile
                  $ unlines
                  $ map prettyAnnotation
                  $ S.toList
                  $ S.map cutAnnotation
                  $ foldMap Exports.toAnnotations pagesToExport
            putStrLn "done"


    let resolveRedirect = resolveRedirectFactory siteId $ AnnsFile.pages unprocessedPages
    -- entity annnotations
        writeEntityAnnotations ::  FilePath ->  (SectionPath -> SectionPath) -> IO ()
        writeEntityAnnotations relsFile cutSectionPath = do
            putStr "Writing section relevance annotations..."
            let cutAnnotation (EntityAnnotation sectionPath entityId rel) =
                  EntityAnnotation (cutSectionPath sectionPath) entityId rel
            writeFile relsFile
                  $ unlines
                  $ map prettyEntityAnnotation
                  $ S.toList
                  $ S.map cutAnnotation
                  $ foldMap (Exports.toEntityAnnotations resolveRedirect) pagesToExport
            putStrLn "done"


    let cutSectionPathArticle (SectionPath pgId _headinglist)  =
            SectionPath pgId mempty
    let cutSectionPathTopLevel  (SectionPath pgId headinglist) =
            SectionPath pgId (take 1 headinglist)


    writeAnnotations        (outpath <.> "hierarchical.qrels")         id
    writeAnnotations        (outpath <.> "article.qrels")              cutSectionPathArticle
    writeAnnotations        (outpath <.> "toplevel.qrels")             cutSectionPathTopLevel
    writeEntityAnnotations  (outpath <.> "hierarchical.entity.qrels")  id
    writeEntityAnnotations  (outpath <.> "article.entity.qrels")       cutSectionPathArticle
    writeEntityAnnotations  (outpath <.> "toplevel.entity.qrels")      cutSectionPathTopLevel



    return ()
