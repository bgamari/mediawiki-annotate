import Data.Proxy
import Control.Monad
import Data.Semigroup hiding (option)
import qualified Data.HashSet as HS
import Options.Applicative
import Codec.Serialise

import CAR.Types
import CAR.ToolVersion

args :: Parser (FilePath, [FilePath], Bool)
args =
    (,,)
    <$> option str (short 'o' <> long "output" <> help "output file")
    <*> some (argument str (help "pages file" <> metavar "PAGES"))
    <*> switch (short 'O' <> long "outlines" <> help "merge outlines files")

main :: IO ()
main = do
    (output, files, outlines) <- execParser' 2 (helper <*> args) mempty
    if outlines
      then catFiles readOutlinesFileWithProvenance output files
      else catFiles readPagesFileWithProvenance output files

catFiles :: (File a, Serialise a)
         => (FilePath -> IO (Provenance, [a])) -> FilePath -> [FilePath] -> IO ()
catFiles read output files = do
    pages <- mapM read files
    let siteProvs = HS.toList $ foldMap (HS.fromList . siteProvenances . fst) pages
        prov = (fst $ head pages) { siteProvenances = siteProvs }
    writeCarFile output prov (foldMap snd pages)
