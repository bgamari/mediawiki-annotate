{-# LANGUAGE MultiWayIf #-}

import Control.Monad
import Data.List (intersperse)
import Data.Maybe
import Data.Monoid

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

import qualified CAR.AnnotationsFile as CAR
import CAR.Types
import CAR.Utils

opts :: Parser (IO ())
opts = subparser
    $  cmd "titles"        dumpTitles
    <> cmd "pages"         dumpPages
    <> cmd "entityids"     dumpEntityIds
    <> cmd "paragraphs"    dumpParagraphs
    <> cmd "paragraphids"  dumpParagraphIds
    <> cmd "sections"      dumpSections
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
        f <$> argument str (help "input file" <> metavar "FILE")
      where
        f inputFile = do
            pages <- readPagesFile inputFile
            mapM_ (T.putStrLn . getPageName . pageName) pages

    dumpSections =
        f <$> argument str (help "input file" <> metavar "FILE")
          <*> flag False True (long "raw" <> help "only section paths - no pagenames")
      where
        f inputFile raw = do
            pages <- readPagesFile inputFile
            let sectionpathlist p = fmap escapeSectionPath
                                  $ pageSectionPaths p
                pageNameStr p = (T.unpack $ getPageName $ pageName p)

            if raw then
                mapM_  (\p -> putStrLn $ unlines $ sectionpathlist p) pages
            else
                mapM_ (\p -> putStrLn $ unlines $ pageNameStr p : sectionpathlist p) pages

    dumpPages =
        f <$> argument str (help "input file" <> metavar "FILE")
          <*> fmap S.fromList (many (argument (PageName . T.pack <$> str)
                                      (metavar "PAGE NAME" <> help "Page name to dump or nothing to dump all")))
          <*> flag anchorOnly withLink (long "links" <> help "Show link targets")
          <*> (many (option (flip pageNameToId <$> (packPageName <$> str)) (long "target" <> short 't' <> help "dump only pages with links to this target page name (and the page itself)")))
          <*> ( HS.fromList <$> many (option (packPageId <$> str) (long "targetids" <> help "dump only pages with links to this target page id (and the page itself)")))
      where
        f :: FilePath -> S.Set PageName -> LinkStyle -> [SiteId -> PageId] -> HS.HashSet PageId-> IO ()
        f inputFile pageNames linkStyle targetPageIds1 targetPageIds2  = do
            siteId <- wikiSite . fst <$> readPagesFileWithProvenance inputFile
            if S.null pageNames
              then do
                  pages <- readPagesFile inputFile
                  let pages' =  filter (searchTargets (targetPageIds siteId)) pages
                  mapM_ printPage pages'
              else do
                  anns <- CAR.openAnnotations inputFile
  --                 siteId <- wikiSite . fst <$> readPagesFileWithProvenance inputFile
                  let pageIds = map (pageNameToId siteId) $ S.toList pageNames
                  mapM_ printPage $ filter (searchTargets (targetPageIds siteId))
                                $ mapMaybe (`CAR.lookupPage` anns) pageIds
          where targetPageIds siteId =
                    HS.fromList (map ($ siteId) targetPageIds1)
                    `HS.union` targetPageIds2
                printPage = putStrLn . prettyPage linkStyle
                searchTargets targets page =
                    if | HS.null targets -> True
                       | (pageId page) `HS.member` targets -> True
                       | otherwise     -> let pageTargets = HS.fromList (pageLinkTargetIds page)
                                          in any (  `HS.member` pageTargets) targets

    dumpEntityIds =
        f <$> argument str (help "input file" <> metavar "FILE")
      where
        f :: FilePath -> IO ()
        f inputFile = do
                pages <- readPagesFile inputFile
                mapM_ printPage pages

          where printPage = putStrLn . entityIdFromPage
                entityIdFromPage page =
                    let pname = unpackPageName $ pageName page
                        pid = unpackPageId $ pageId page
                    in pid <> "\t" <> pname

    dumpParagraphs =
        f <$> argument str (help "input paragraph file" <> metavar "FILE")
          <*> flag anchorOnly withLink (long "links" <> help "Show link targets")
      where
        f :: FilePath -> LinkStyle -> IO ()
        f inputFile linkStyle = do
                paragraphs <- readParagraphsFile inputFile
                mapM_ printParagraph paragraphs

          where printParagraph = putStrLn . prettyParagraph linkStyle

    dumpParagraphIds =
        f <$> argument str (help "input paragraph file" <> metavar "FILE")
      where
        f :: FilePath -> IO ()
        f inputFile  = do
                paragraphs <- readParagraphsFile inputFile
                mapM_ printParagraph paragraphs

          where printParagraph (Paragraph paraId' _) = putStrLn $ unpackParagraphId paraId'

    histogramHeadings =
        f <$> argument str (help "input file" <> metavar "FILE")
      where
        f inputFile = do
            pages <- readPagesFile inputFile
            TL.putStrLn $ TB.toLazyText
                $ mconcat
                $ intersperse (TB.singleton '\n')
                $ map (\(SectionHeading h, n) -> TB.decimal n<>TB.singleton '\t'<>TB.fromText h)
                $ HM.toList
                $ HM.fromListWith (+)
                $ map (\h -> (h,1::Int))
                $ foldMap sectionHeadings
                $ foldMap pageSkeleton pages

sectionHeadings :: PageSkeleton -> [SectionHeading]
sectionHeadings (Section h _ children) = h : foldMap sectionHeadings children
sectionHeadings _ = []

main :: IO ()
main = join $ execParser $ info (helper <*> opts) mempty
