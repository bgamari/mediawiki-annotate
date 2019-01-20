import qualified Data.HashSet as HS
import Options.Applicative
import Codec.Serialise
import qualified Data.Text as T
import Data.Maybe

import CAR.Types
import CAR.ToolVersion

data MergeMode = MergePages | MergeParagraphs | MergeOutlines

args :: Parser (FilePath, [FilePath], MergeMode, Maybe T.Text)
args =
    (,,,)
    <$> option str (short 'o' <> long "output" <> help "output file")
    <*> some (argument str (help "pages file" <> metavar "CBOR-Files"))
    <*> mergeMode
    <*> optional (option str (long "data-release-name" <> metavar "RELEASE" <> help "overwrite data release name"))
  where mergeMode = flag' MergeParagraphs (short 'P' <> long "paragraphs" <> help "merge paragraphs files")
                  <|> flag' MergeOutlines (short 'O' <> long "outlines" <> help "merge outlines files")
                  <|> pure MergePages


main :: IO ()
main = do
    (output, files, mergeMode, newDataReleaseName) <- execParser' 2 (helper <*> args) mempty
    case mergeMode of
      MergePages -> catFiles readPagesFileWithProvenance output files newDataReleaseName
      MergeParagraphs -> catFiles readParagraphsFileWithProvenance output files newDataReleaseName
      MergeOutlines -> catFiles readOutlinesFileWithProvenance output files newDataReleaseName

catFiles :: (File a, Serialise a)
         => (FilePath -> IO (Provenance, [a])) -> FilePath -> [FilePath] -> Maybe T.Text -> IO ()
catFiles readF output files newDataReleaseName = do
    pages <- mapM readF files
    let siteProvs = HS.toList $ foldMap (HS.fromList . siteProvenances . fst) pages
        prov = fst $ head pages
        prov' = prov { siteProvenances = siteProvs
                     , dataReleaseName = fromMaybe (dataReleaseName prov) newDataReleaseName }
    writeCarFile output prov' (foldMap snd pages)
