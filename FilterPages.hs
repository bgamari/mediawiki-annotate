import Data.Monoid
import qualified Data.Text as T
import System.IO
import qualified Data.HashSet as HS
import Options.Applicative
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as BSB
import Data.Hashable
import CAR.Types

opts :: Parser (FilePath, FilePath, IO (Page -> Bool))
opts =
    (,,)
    <$> argument str (help "input file")
    <*> option str (short 'o' <> long "output")
    <*> predicate
  where
    predicate :: Parser (IO (Page -> Bool))
    predicate = testPred <|> trainPred <|> namePred
    testPred = flag' f (long "test")
      where f = pure $ even . hash . pageName
    trainPred = flag' f (long "train")
      where f = pure $ odd . hash . pageName
    namePred = option (f <$> str) (long "name-set")
      where f fname = do
                pages <- HS.fromList . map (PageName . T.pack) . lines <$> readFile fname
                return $ (`HS.member` pages) . pageName

main :: IO ()
main = do
    (inputFile, outputFile, getPredicate) <- execParser $ info (helper <*> opts) mempty
    predicate <- getPredicate
    pages <- decodeCborList <$> BSL.readFile inputFile
    withFile outputFile WriteMode $ \h ->
        BSB.hPutBuilder h $ encodeCborList $ filter predicate pages
