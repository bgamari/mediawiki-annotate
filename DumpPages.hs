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
import Options.Applicative
import qualified Data.ByteString.Lazy as BSL

import CAR.AnnotationsFile
import CAR.Types

opts :: Parser (IO ())
opts = subparser
    $  cmd "titles"        dumpTitles
    <> cmd "pages"         dumpPages
    <> cmd "sections"      dumpSections
    <> cmd "hist-headings" histogramHeadings
  where
    cmd name action = command name (info (helper <*> action) fullDesc)
    dumpTitles =
        f <$> argument str (help "input file" <> metavar "FILE")
      where
        f inputFile = do
            pages <- decodeCborList <$> BSL.readFile inputFile
            mapM_ (T.putStrLn . getPageName . pageName) pages

    dumpSections =
        f <$> argument str (help "input file" <> metavar "FILE")
      where
        f inputFile = do
            pages <- decodeCborList <$> BSL.readFile inputFile
            let sectionpathlist p = fmap escapeSectionPath
                                  $ listSections p
            let pageNameStr p = (T.unpack $ getPageName $ pageName p)

            mapM_ (\p -> putStrLn $ unlines $ pageNameStr p : sectionpathlist p) pages
        listSections :: Page -> [SectionPath]
        listSections (Page _ pageId skeleton) =
             fmap (\sp -> (SectionPath pageId sp) )
             $ foldMap go skeleton
          where
            go :: PageSkeleton -> [[HeadingId]]
            go (Section _ sectionId children) =
                let childSectionPaths = foldMap go children
                in if null childSectionPaths
                then [[sectionId]]
                else fmap (\childPath -> sectionId : childPath)  childSectionPaths
            go (Para {}) = []

    dumpPages =
        f <$> argument str (help "input file" <> metavar "FILE")
          <*> fmap S.fromList (many (argument (pageNameToId . PageName . T.pack <$> str)
                                      (metavar "PAGE NAME" <> help "Page name to dump or nothing to dump all")))
          <*> flag anchorOnly withLink (long "links" <> help "Show link targets")
      where
        f :: FilePath -> S.Set PageId -> LinkStyle -> IO ()
        f inputFile pageNames linkStyle
          | S.null pageNames = do
                pages <- decodeCborList <$> BSL.readFile inputFile
                mapM_ printPage pages
          | otherwise = do
                anns <- openAnnotations inputFile
                mapM_ printPage $ mapMaybe (`lookupPage` anns) (S.toList pageNames)

          where printPage = putStrLn . prettyPage linkStyle

    histogramHeadings =
        f <$> argument str (help "input file" <> metavar "FILE")
      where
        f inputFile = do
            pages <- decodeCborList <$> BSL.readFile inputFile
            TL.putStrLn $ TB.toLazyText
                $ mconcat
                $ intersperse (TB.singleton '\n')
                $ map (\(SectionHeading h, n) -> TB.decimal n<>TB.singleton '\t'<>TB.fromText h)
                $ HM.toList
                $ HM.fromListWith (+)
                $ map (\h -> (h,1))
                $ foldMap sectionHeadings pages

sectionHeadings :: PageSkeleton -> [SectionHeading]
sectionHeadings (Section h _ children) = h : foldMap sectionHeadings children
sectionHeadings (Para _) = []

main :: IO ()
main = join $ execParser $ info (helper <*> opts) mempty
