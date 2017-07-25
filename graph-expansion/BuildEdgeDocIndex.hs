import Control.Applicative
import Data.Monoid

import Options.Applicative
import EdgeDocCorpus
import EdgeDocIndex
import CAR.Types

args :: Parser (FilePath, FilePath)
args = (,)
    <$> option str (long "output" <> short 'o' <> help "output index path")
    <*> argument str (help "articles file")

main :: IO ()
main = do
    (outputPath, articlesPath) <- execParser $ info (helper <*> args) mempty
    pages <- readCborList articlesPath
    _ <- indexEdgeDocs outputPath (pagesToEdgeDocs pages)
    return ()
