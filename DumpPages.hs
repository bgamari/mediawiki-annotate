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
    $  command "titles" (info dumpTitles fullDesc)
    <> command "pages" (info  dumpPages fullDesc)
    <> command "hist-headings" (info  histogramHeadings fullDesc)
  where
    dumpTitles =
        f <$> argument str (help "input file" <> metavar "FILE")
      where
        f inputFile = do
            pages <- decodeCborList <$> BSL.readFile inputFile
            mapM_ (T.putStrLn . getPageName . pageName) pages

    dumpPages =
        f <$> argument str (help "input file" <> metavar "FILE")
          <*> fmap S.fromList (many (argument (PageName . T.pack <$> str)
                                      (metavar "PAGE NAME" <> help "Page name to dump or nothing to dump all")))
      where
        f :: FilePath -> S.Set PageName -> IO ()
        f inputFile pageNames
          | S.null pageNames = do
                pages <- decodeCborList <$> BSL.readFile inputFile
                mapM_ printPage pages
          | otherwise = do
                anns <- openAnnotations inputFile
                mapM_ printPage $ mapMaybe (`lookupPage` anns) (S.toList pageNames)

        printPage = putStrLn . prettyPage

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
