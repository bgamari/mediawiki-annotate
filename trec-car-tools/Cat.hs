import Data.Proxy
import Control.Monad
import Data.Semigroup hiding (option)
import qualified Data.HashSet as HS
import Options.Applicative
import Codec.Serialise
import qualified Data.Text as T
import Data.Maybe

import CAR.Types
import CAR.ToolVersion

args :: Parser (FilePath, [FilePath], Bool, Maybe T.Text)
args =
    (,,,)
    <$> option str (short 'o' <> long "output" <> help "output file")
    <*> some (argument str (help "pages file" <> metavar "PAGES"))
    <*> switch (short 'O' <> long "outlines" <> help "merge outlines files")
    <*> optional (option str (long "data-release-name" <> metavar "RELEASE" <> help "overwrite data release name"))
main :: IO ()
main = do
    (output, files, outlines, newDataReleaseName) <- execParser' 2 (helper <*> args) mempty
    if outlines
      then catFiles readOutlinesFileWithProvenance output files newDataReleaseName
      else catFiles readPagesFileWithProvenance output files newDataReleaseName

catFiles :: (File a, Serialise a)
         => (FilePath -> IO (Provenance, [a])) -> FilePath -> [FilePath] -> Maybe T.Text -> IO ()
catFiles read output files newDataReleaseName = do
    pages <- mapM read files
    let siteProvs = HS.toList $ foldMap (HS.fromList . siteProvenances . fst) pages
        prov = fst $ head pages
        prov' = prov { siteProvenances = siteProvs
                     , dataReleaseName = fromMaybe (dataReleaseName prov) newDataReleaseName }
    writeCarFile output prov' (foldMap snd pages)
