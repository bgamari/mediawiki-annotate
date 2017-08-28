import Control.Monad
import Data.List (intersperse)
import Data.Maybe
import Data.Monoid

import qualified Data.Set as S
import qualified Data.HashMap.Strict as HM
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
      where
        f inputFile = do
            pages <- readPagesFile inputFile
            let sectionpathlist p = fmap escapeSectionPath
                                  $ pageSectionPaths p
            let pageNameStr p = (T.unpack $ getPageName $ pageName p)

            mapM_ (\p -> putStrLn $ unlines $ pageNameStr p : sectionpathlist p) pages

    dumpPages =
        f <$> argument str (help "input file" <> metavar "FILE")
          <*> fmap S.fromList (many (argument (PageName . T.pack <$> str)
                                      (metavar "PAGE NAME" <> help "Page name to dump or nothing to dump all")))
          <*> flag anchorOnly withLink (long "links" <> help "Show link targets")
      where
        f :: FilePath -> S.Set PageName -> LinkStyle -> IO ()
        f inputFile pageNames linkStyle
          | S.null pageNames = do
                pages <- readPagesFile inputFile
                mapM_ printPage pages
          | otherwise = do
                anns <- CAR.openAnnotations inputFile
                siteId <- wikiSite . fst <$> readPagesFile' inputFile
                let pageIds = map (pageNameToId siteId) $ S.toList pageNames
                mapM_ printPage $ mapMaybe (`CAR.lookupPage` anns) pageIds

          where printPage = putStrLn . prettyPage linkStyle

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
sectionHeadings (Para _) = []
sectionHeadings (Image{}) = []

main :: IO ()
main = join $ execParser $ info (helper <*> opts) mempty
