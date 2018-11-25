{-# LANGUAGE MultiWayIf #-}

import Control.Monad
import Data.List (intersperse)
import Data.Maybe
import Data.Monoid
import Data.List

import qualified Data.Set as S
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import Options.Applicative hiding (action)
import qualified Codec.Serialise as CBOR
import qualified Data.ByteString.Lazy as BSL

import qualified CAR.TocFile as TocFile
import qualified CAR.AnnotationsFile as CAR
import qualified CAR.NameToIdMap as CARN
import CAR.Types
import CAR.Utils
import CAR.ToolVersion
import CAR.Types.Files

opts :: Parser (IO ())
opts = subparser
    $  cmd "titles"        dumpTitles
    <> cmd "page-ids"      dumpPageIds
    <> cmd "meta"          dumpMeta
    <> cmd "pages"         dumpPages
    <> cmd "entityids"     dumpEntityIds
    <> cmd "paragraphs"    dumpParagraphs
    <> cmd "paragraphids"  dumpParagraphIds
    <> cmd "filter-paragraphids"  filterParagraphIds
    <> cmd "paragraphids-pages"  paragraphIdsInPages
    <> cmd "section-ids"      dumpSectionIds
    <> cmd "outlines"      dumpOutlines
    <> cmd "hist-headings" histogramHeadings
    <> cmd "dump-header"   dumpHeader
  where
    cmd name action = command name (info (helper <*> action) fullDesc)
    dumpHeader =
        f <$> argument str (help "input file" <> metavar "FILE")
      where
        f inputFile = do
            hdr <- CBOR.deserialise <$> BSL.readFile inputFile
            print (hdr :: Header)

    dumpTitles =
        f <$> pagesFromFile
      where
        f getPages = do
            pages <- getPages
            mapM_ (T.putStrLn . getPageName . pageName) pages

    dumpPageIds =
        f <$> pagesFromFile
      where
        f getPages = do
            pages <- getPages
            mapM_ (putStrLn . unpackPageId . pageId) pages


    dumpSectionIds =
        f <$> pagesFromFile
          <*> flag False True (long "raw" <> help "only section paths - no pagenames")
          <*> flag False True (long "internal" <> help "also internal sections")
      where
        f getPages raw internal = do
            pages <- getPages
            let sectionpathlist p = fmap escapeSectionPath
                                  $ pageSectionPaths p
                pageNameStr p = (T.unpack $ getPageName $ pageName p)

            if raw then
                mapM_  (\p -> putStrLn $ unlines $ sectionpathlist p) pages
            else
                mapM_ (\p -> putStrLn $ unlines $ pageNameStr p : sectionpathlist p) pages

    dumpOutlines =
        f <$> pagesFromFile
          <*> flag False True (long "page-id" <> help "also print page id")
      where
        f getPages withPageId = do
            pages <- getPages

            let indentHeadings p = [ (length headinglist, last headinglist)
                                   |  (_, headinglist, _) <- pageSections p
                                   , headinglist /= []
                                   ]  -- [(SectionPath, [SectionHeading], [PageSkeleton])]

                pageNameStr p = (T.unpack $ getPageName $ pageName p)
                pageIdStr p = (unpackPageId $ pageId p)

                formatIndentHeadings :: (Int, SectionHeading) -> String
                formatIndentHeadings (level, headingtext) = (replicate level '\t') <> T.unpack (getSectionHeading headingtext)


                prettyPage p = unlines $
                    ( if withPageId then [pageIdStr p] else [] )
                    ++ [ pageNameStr p ]
                    ++ fmap formatIndentHeadings (indentHeadings p)
                    ++ [""]

            putStrLn $ unlines $ map prettyPage pages


    dumpPages =
        f <$> pagesFromFile
          <*> flag anchorOnly withLink (long "links" <> help "Show link targets")
      where
        f :: IO [Page] -> LinkStyle -> IO ()
        f getPages linkStyle = do
            pages <- getPages
            let printPage = putStrLn . prettyPage linkStyle
            mapM_ printPage pages

    dumpMeta =
        f <$> pagesFromFile
      where
        f :: IO [Page] -> IO ()
        f getPages = do
            pages <- getPages
            let printPage = putStrLn . prettyMeta
            mapM_ printPage pages

    paragraphIdsInPages =
        f <$> pagesFromFile
      where
        f :: IO [Page] -> IO ()
        f getPages = do
            pages <- getPages
            mapM_ printParagraphId pages
          where
                printParagraphId page = mapM_ printParagraphIdPara $ pageParas page

                printParagraphIdPara (Paragraph paraId' _) = putStrLn $ unpackParagraphId paraId'


    dumpEntityIds =
        f <$> pagesFromFile
      where
        f :: IO [Page] -> IO ()
        f getPages = do
                pages <- getPages
                mapM_ printPage pages

          where printPage = putStrLn . entityIdFromPage
                entityIdFromPage page =
                    let pname = unpackPageName $ pageName page
                        pid = unpackPageId $ pageId page
                    in pid <> "\t" <> pname

    dumpParagraphs =
        f <$> argument str (help "input paragraph file" <> metavar "FILE")
          <*> flag anchorOnly withLink (long "links" <> help "Show link targets")
          <*> many (option (packParagraphId <$> str) (long "paragraph" <> short 'p' <> help "dump only paragraphs with the given paragraph id"))
      where
        f :: FilePath -> LinkStyle -> [ParagraphId] -> IO ()
        f inputFile linkStyle paraIds = do
            paragraphs <- if null paraIds
              then readParagraphsFile inputFile
              else do paras <- TocFile.open $ TocFile.IndexedCborPath inputFile
                      return $ mapMaybe (`TocFile.lookup` paras) paraIds
            mapM_ printParagraph paragraphs

          where printParagraph = putStrLn . prettyParagraph linkStyle

    dumpParagraphIds =
        f <$> argument str (help "input paragraph file" <> metavar "FILE")
      where
        f :: FilePath -> IO ()
        f inputFile  = do
                paragraphs <- readParagraphsFile inputFile
                mapM_ printParagraphId paragraphs

          where printParagraphId (Paragraph paraId' _) = putStrLn $ unpackParagraphId paraId'

    filterParagraphIds =
        f <$> argument str (help "input paragraph file" <> metavar "FILE")
          <*> option (TocFile.IndexedCborPath <$> str) (long "para2" <> help "dump only paragraph ids that are also in this file")
          <*> switch (short 'n' <> long "negate" <> help "invert matching logic")
      where
        f :: FilePath -> TocFile.IndexedCborPath ParagraphId Paragraph -> Bool -> IO ()
        f inputFile para2File negate = do
                paragraphs <- readParagraphsFile inputFile
                para2 <- TocFile.open para2File
                let paragraphs' = filter (\ (Paragraph pid _) ->  negate /= isJust (TocFile.lookup pid para2) ) paragraphs
                mapM_ printParagraphId paragraphs'

          where printParagraphId (Paragraph paraId' _) = putStrLn $ unpackParagraphId paraId'

    histogramHeadings =
        f <$> pagesFromFile
      where
        f getPages = do
            pages <- getPages
            TL.putStrLn $ TB.toLazyText
                $ mconcat
                $ intersperse (TB.singleton '\n')
                $ map (\(SectionHeading h, n) -> TB.decimal n<>TB.singleton '\t'<>TB.fromText h)
                $ HM.toList
                $ HM.fromListWith (+)
                $ map (\h -> (h,1::Int))
                $ foldMap sectionHeadings
                $ foldMap pageSkeleton pages


readFilteredPages :: S.Set PageName    -- ^ set of page names to read
                  -> S.Set PageId    -- ^ set of page names to read
                  -> CAR.PageBundle          -- ^ pages or outlines file
                  -> [Page]
readFilteredPages pageNames pageIds pageBundle =
   if S.null pageNames && S.null pageIds  then
     CAR.bundleAllPages pageBundle
   else
     let pageIds' = pageIds <>  (CAR.bundleLookupAllPageNames pageBundle) pageNames
     in mapMaybe (CAR.bundleLookupPage pageBundle) ( S.toList  pageIds')

pagesFromFile :: Parser (IO [Page])
pagesFromFile =
    f <$> argument str (help "input file" <> metavar "FILE")
      <*> fmap S.fromList (many (option  (packPageName  <$> str) (short 'p' <> long "page" <> metavar "PAGE NAME" <> help "Page name to dump or nothing to dump all")))
      <*> fmap S.fromList (many (option  (packPageId  <$> str) (short 'P' <> long "pageid" <> metavar "PAGE ID " <> help "Page id to dump or nothing to dump all")))
      <*> (many (option (packPageName <$> str) (long "target" <> short 't' <> help "dump only pages with links to this target page name (and the page itself)")))
      <*> ( HS.fromList <$> many (option (packPageId <$> str) (long "targetids" <> short 'T'  <> help "dump only pages with links to this target page id (and the page itself)")))
      <*> ( HS.fromList <$> many (option (packPageName <$> str)  (long "redirect" <> short 'r' <> help "dump only pages with redirects from this page name")))
  where
    f :: FilePath -> S.Set PageName -> S.Set PageId -> [PageName] -> HS.HashSet PageId -> HS.HashSet PageName -> IO [Page]
    f inputFile pageNames pageIds targetPageNames1 targetPageIds2 redirectPageNames = do
        pageBundle <- CAR.openPageBundle inputFile
        let targetPageIds1 = S.toList $ CAR.bundleLookupAllPageNames pageBundle targetPageNames1

            targetPageIds =
                HS.fromList targetPageIds1
                `HS.union` targetPageIds2
            searchTargets targets page =
                if | HS.null targets -> True
                   | (pageId page) `HS.member` targets -> True
                   | otherwise     -> let pageTargets = HS.fromList (pageLinkTargetIds page)
                                      in any (  `HS.member` pageTargets) targets
            redirectTargets :: HS.HashSet PageName -> Page -> Bool
            redirectTargets redirects page =
                if | HS.null redirects -> True
                   | Just pageRedirects <- getMetadata _RedirectNames (pageMetadata page)
                        -> let pageRedirectSet = HS.fromList pageRedirects
                           in any (  `HS.member` pageRedirectSet) redirects
                   | otherwise -> False

            pages = readFilteredPages pageNames pageIds pageBundle
        return $ filter (redirectTargets redirectPageNames)
               $ filter (searchTargets targetPageIds) pages

sectionHeadings :: PageSkeleton -> [SectionHeading]
sectionHeadings (Section h _ children) = h : foldMap sectionHeadings children
sectionHeadings _ = []

main :: IO ()
main = join $ execParser' 1 (helper <*> opts) mempty
