import Control.Monad
import Data.Maybe
import Data.Monoid

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Options.Applicative
import qualified Data.ByteString.Lazy as BSL

import CAR.AnnotationsFile
import CAR.Types

opts :: Parser (IO ())
opts = subparser
    $  command "titles" (info dumpTitles fullDesc)
    <> command "pages" (info  dumpPages fullDesc)
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

main :: IO ()
main = join $ execParser $ info (helper <*> opts) mempty
