import Data.Monoid
import System.IO
import Options.Applicative
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as BSB
import qualified Text.Trifecta as Tri

import CAR.Types
import FilterPred

opts :: Parser (FilePath, FilePath, Pred)
opts =
    (,,)
    <$> argument str (help "input file" <> metavar "ANNOTATIONS FILE")
    <*> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")
    <*> argument predicate (metavar "PRED" <> help "Predicate")
  where
    predicate = do
        s <- str
        case Tri.parseString FilterPred.pred mempty s of
          Tri.Success p -> return p
          Tri.Failure e -> fail $ show e

main :: IO ()
main = do
    (inputFile, outputFile, predicate) <- execParser $ info (helper <*> opts) mempty
    pages <- decodeCborList <$> BSL.readFile inputFile
    withFile outputFile WriteMode $ \h ->
        BSB.hPutBuilder h $ encodeCborList $ filter (interpret predicate) pages
