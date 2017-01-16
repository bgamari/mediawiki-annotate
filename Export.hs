{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.Maybe
import Data.Monoid
import System.IO

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Builder as BSB
import System.FilePath

import Options.Applicative

import CAR.AnnotationsFile
import CAR.Types
import CAR.CarExports as Exports

options :: Parser (FilePath, FilePath, [PageName])
options =
    (,,) <$> argument str (help "annotations file" <> metavar "FILE")
        <*> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")
        <*> many (option (PageName . T.pack <$> str)
                         (short 'p' <> long "page"
                          <> metavar "PAGE NAME" <> help "Export only this page"))


main :: IO ()
main = do
    (path, outpath, names) <- execParser $ info (helper <*> options) mempty
    anns <- openAnnotations path
    let pagesToExport
          | null names = pages anns
          | otherwise  = mapMaybe (`lookupPage` anns) names
        {-# INLINE pagesToExport #-}

    when (not $ null names) $ do
        putStr "Writing articles..."
        let articleFile = outpath <.> "articles"
        withFile articleFile WriteMode $ \h ->
            BSB.hPutBuilder h $ encodeCborList pagesToExport
        putStrLn "done"

    putStr "Writing outlines..."
    let skeletonFile = outpath <.> "outlines"
    withFile skeletonFile WriteMode $ \h ->
        BSB.hPutBuilder h $ encodeCborList $ map toStubSkeleton pagesToExport
    putStrLn "done"

    putStr "Writing paragraphs..."
    let paragraphsFile = outpath <.> "paragraphs"
    let sortIt = map snd . M.toAscList . foldMap (\para -> M.singleton (paraId para) para)
    withFile paragraphsFile WriteMode $ \h ->
        BSB.hPutBuilder h $ encodeCborList $ sortIt $ concatMap toParagraphs pagesToExport
    putStrLn "done"



    -- paragraph annotations
    let writeAnnotations ::  FilePath -> [Page] ->  (SectionPath -> SectionPath) -> IO ()
        writeAnnotations relsFile pages cutSectionPath = do
            putStr "Writing section relevance annotations..."
            let cutAnnotation (Annotation sectionPath paraId rel) =
                  Annotation (cutSectionPath sectionPath) paraId rel
            withFile relsFile WriteMode $ \h ->
                  hPutStr h $ unlines $ map prettyAnnotation $ map cutAnnotation $ concatMap Exports.toAnnotations pagesToExport
            putStrLn "done"


    -- entity annnotations
    let writeEntityAnnotations ::  FilePath -> [Page] ->  (SectionPath -> SectionPath) -> IO ()
        writeEntityAnnotations relsFile pages cutSectionPath = do
            putStr "Writing section relevance annotations..."
            let cutAnnotation (EntityAnnotation sectionPath entityId rel) =
                  EntityAnnotation (cutSectionPath sectionPath) entityId rel
            withFile relsFile WriteMode $ \h ->
                  hPutStr h $ unlines $ map prettyEntityAnnotation $ map cutAnnotation $ concatMap Exports.toEntityAnnotations pagesToExport
            putStrLn "done"


    let cutSectionPathArticle (SectionPath pageId headinglist)  =
            SectionPath pageId mempty
    let cutSectionPathTopLevel  (SectionPath pageId headinglist) =
            SectionPath pageId (take 1 headinglist)


    writeAnnotations  (outpath <.> "hierarchical.qrels")  pagesToExport id
    writeAnnotations  (outpath <.> "article.qrels")  pagesToExport cutSectionPathArticle
    writeAnnotations  (outpath <.> "toplevel.qrels")  pagesToExport cutSectionPathTopLevel
    writeEntityAnnotations  (outpath <.> "hierarchical.entity.qrels")  pagesToExport id
    writeEntityAnnotations  (outpath <.> "article.entity.qrels")  pagesToExport cutSectionPathArticle
    writeEntityAnnotations  (outpath <.> "toplevel.entity.qrels")  pagesToExport cutSectionPathTopLevel



--
--
--     putStr "Writing section relevance annotations..."
--     let relsFile = outpath <.> "hierarchical.qrels"
--     withFile relsFile WriteMode $ \h ->
--           hPutStr h $ unlines $ map prettyAnnotation $ concatMap Exports.toAnnotations pagesToExport
--     putStrLn "done"
--
--     putStr "Writing article relevance annotations..."
--     let relsFile = outpath <.> "article.qrels"
--     withFile relsFile WriteMode $ \h ->
--         let cutSectionPath (Annotation (SectionPath pageId headinglist) paraId rel) =
--               Annotation (SectionPath pageId mempty) paraId rel
--         in hPutStr h $ unlines $ map prettyAnnotation $ map cutSectionPath $ concatMap Exports.toAnnotations pagesToExport
--     putStrLn "done"
--
--     putStr "Writing top level section relevance annotations..."
--     let relsFile = outpath <.> "toplevel.qrels"
--     withFile relsFile WriteMode $ \h ->
--           let cutSectionPath (Annotation (SectionPath pageId headinglist) paraId rel) =
--                Annotation (SectionPath pageId (take 1 headinglist)) paraId rel
--           in hPutStr h $ unlines $ map prettyAnnotation $ map cutSectionPath $ concatMap Exports.toAnnotations pagesToExport
--     putStrLn "done"
--
--
--
--     putStr "Writing section relevance entity annotations..."
--     let relsFile = outpath <.> "hierarchical.entity.qrels"
--     withFile relsFile WriteMode $ \h ->
--           hPutStr h $ unlines $ map prettyAnnotation $ concatMap Exports.toEntityAnnotations pagesToExport
--     putStrLn "done"
--
--     putStr "Writing article relevance entity annotations..."
--     let relsFile = outpath <.> "article.entity.qrels"
--     withFile relsFile WriteMode $ \h ->
--         let cutSectionPath (EntityAnnotation (SectionPath pageId headinglist) entityId rel) =
--               EntityAnnotation (SectionPath pageId mempty) entityId rel
--         in hPutStr h $ unlines $ map prettyAnnotation $ map cutSectionPath $ concatMap Exports.toEntityAnnotations pagesToExport
--     putStrLn "done"
--
--     putStr "Writing top level section entity relevance annotations..."
--     let relsFile = outpath <.> "toplevel.entity.qrels"
--     withFile relsFile WriteMode $ \h ->
--           let cutSectionPath (EntityAnnotation (SectionPath pageId headinglist) entityId rel) =
--                EntityAnnotation (SectionPath pageId (take 1 headinglist)) entityId rel
--           in hPutStr h $ unlines $ map prettyAnnotation $ map cutSectionPath $ concatMap Exports.toEntityAnnotations pagesToExport
--     putStrLn "done"

    return ()
