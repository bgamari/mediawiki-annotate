import qualified Data.Text.IO as T
import Options.Applicative
import qualified Data.ByteString.Lazy as BSL
import CAR.Types

opts :: Parser (FilePath, Page -> IO ())
opts =
    (,)
    <$> argument str (help "input file")
    <*> what
  where
    what :: Parser (Page -> IO ())
    what = dumpTitles <|> dumpAll

    dumpTitles = flag' f (long "titles")
      where f = T.putStrLn . getPageName . pageName
    dumpAll = flag' f (long "all")
      where f = putStrLn . prettyPage

main :: IO ()
main = do
    (inputFile, dumpIt) <- execParser $ info (helper <*> opts) mempty
    pages <- decodeCborList <$> BSL.readFile inputFile
    mapM_ dumpIt pages
