#!/usr/bin/env runghc

-- Usage: (from car_team_results)
-- ./bin/appendix-table  (*/*.eval.gz|*/*.eval) | pandoc --from json --to html - -o results_table.html --standalone -M "title=TREC Car Results"

import Numeric
import Data.Char
import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.List.Split
import System.Environment
import System.FilePath
import Codec.Compression.GZip
import System.FilePath.Glob
import qualified Data.Text as T
import Options.Applicative

import Text.Tabular


import qualified Text.Pandoc.Definition as Pandoc
import qualified Text.Pandoc.Class
import qualified Text.Pandoc.Options
import qualified Text.Pandoc.Writers.HTML
import Data.Aeson



opts :: Parser ([FilePath], FilePath, String)
opts =
    (,,)
    <$> some (argument str (metavar "confirmfiles" <> help "A glob pattern for confirm files uploaded with the submission"))
    <*> option str (short 'o' <> long "out" <> help "output file")
    <*> option str (short 't' <> long "title" <> help "page title. will be appended with \"Entity/Passage task\"")

-- Expect file names to be of this format:
--  $methodname.$assess.$runType.eval*
-- example: mpii-nn6_pos.automatic.psg.eval.gz

readConfirm ::  FilePath -> IO [Pandoc.Block]
readConfirm  path  =
    parse <$> BSL.readFile path
  where
--     parse :: BSL.ByteString -> [Pandoc.Block]
    parse x =
        mapMaybe (convertLine . BSL.unpack) $ BSL.lines x
            where
                convertLine :: String -> Maybe Pandoc.Block
                convertLine line | "Run " `isPrefixOf` line = Just $ Pandoc.Header 2 mempty [Pandoc.Str line]
                convertLine line | null line                 = Nothing
                convertLine line | "\t\t" `isPrefixOf` line = Just $ Pandoc.Para [Pandoc.Str line]
                convertLine line | "\t" `isPrefixOf` line   = Just $ Pandoc.Para [Pandoc.Strong [ Pandoc.Str line]]
                convertLine _                               = Nothing


main :: IO ()
main = do
    (evalGlobs, output, title) <- execParser $ info (helper <*> opts) mempty
    evalFiles <- concat <$> mapM glob evalGlobs
    blocks <-  concat <$> mapM readConfirm evalFiles
    let pandocPage =  Pandoc.Pandoc Pandoc.nullMeta ( [pandocHeader (title <> " Run Description")]  ++ blocks   )


    let htmlOpts = Text.Pandoc.Options.def {Text.Pandoc.Options.writerPreferAscii = True}
    htmlText <- Text.Pandoc.Class.runIOorExplode $ Text.Pandoc.Writers.HTML.writeHtml5String htmlOpts pandocPage
    writeFile output $ T.unpack htmlText
    return ()

pandocHeader :: String -> Pandoc.Block
pandocHeader text = Pandoc.Header 1 mempty [Pandoc.Str text ]


