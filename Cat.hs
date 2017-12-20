import Control.Monad
import Data.Semigroup hiding (option)
import CAR.Types
import Options.Applicative

args :: Parser (FilePath, [FilePath])
args =
    (,)
    <$> option str (short 'o' <> long "output" <> help "output file")
    <*> some (argument str (help "pages file" <> metavar "PAGES"))

main :: IO ()
main = do
    (output, files) <- execParser $ info (helper <*> args) mempty
    pages <- mapM readPagesFileWithProvenance files
    let prov = fst $ head pages
    unless (all (== prov) $ map fst pages)
        $ fail $ unlines $ "provenance mismatch:"
                         : [ fname ++ "\t" ++ show p
                           | (fname, p) <- zip files (map fst pages)
                           ]
    writeCarFile output prov (foldMap snd pages)
