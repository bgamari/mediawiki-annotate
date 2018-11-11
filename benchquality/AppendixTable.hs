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
import qualified Data.Text as T

import Text.Tabular
--import Text.Tabular.AsciiArt
--import Text.Tabular.Html
--import Text.Tabular.Latex

import qualified Text.Pandoc.Definition as Pandoc
import qualified Text.Pandoc.Class
import qualified Text.Pandoc.Options
import qualified Text.Pandoc.Writers.HTML
import Data.Aeson

data RunType = Entity | Passage
             deriving (Eq, Ord, Show)
data AssessmentMethod = Automatic | Lenient | Manual
                      deriving (Eq, Ord, Enum, Bounded, Show)
newtype RunName = RunName { getRunName :: String }
                deriving (Eq, Ord, Show)
data Metric = MAP | RPrec | RecipRank | NDCG
            deriving (Eq, Ord, Enum, Bounded, Show)
newtype Query = Query BS.ByteString
              deriving (Show)


-- Expect file names to be of this format:
--  $methodname.$assess.$runType.eval*
-- example: mpii-nn6_pos.automatic.psg.eval.gz

readEval :: FilePath -> IO [(RunName, RunType, AssessmentMethod, Query, Metric, Double)]
readEval path =
    if ".gz" `isSuffixOf` path then
        parse . decompress <$> BSL.readFile path
    else
        parse <$> BSL.readFile path
  where
    parse :: BSL.ByteString -> [(RunName, RunType, AssessmentMethod, Query, Metric, Double)]
    parse x =
        let parseLine (metricName : query : score : [])
              | Just metric <- parseMetric $ BSL.unpack $ BSL.takeWhile (not . isSpace) metricName
              = let s = read $ BSL.unpack score
                in Just (runName, runType, assess, Query $ BSL.toStrict query, metric, s)
            parseLine _ = Nothing

            parts = splitOn "." $ takeFileName path
            runName = RunName $ parts !! 0
            runType = case parts !! 2 of
                        "psg" -> Passage
                        "entity" -> Entity
            assess = case parts !! 1 of
                       "automatic" -> Automatic
                       "lenient" -> Lenient
                       "manual" -> Manual
                       s        -> error $ "unknown assessment type "++show s
        in mapMaybe (parseLine . BSL.split '\t') $ BSL.lines x

    parseMetric name =
        case name of
          "map"        -> Just MAP
          "Rprec"      -> Just RPrec
          "recip_rank" -> Just RecipRank
          "ndcg"       -> Just NDCG
          _            -> Nothing

mean :: RealFrac a => [a] -> a
mean xs = sum xs / realToFrac (length xs)

stddev :: RealFloat a => [a] -> a
stddev xs = sqrt $ mean [ (x - m)^2 | x <- xs ]
  where m = mean xs

stderr :: RealFloat a => [a] -> a
stderr xs = stddev xs / sqrt (realToFrac $ length xs)

main :: IO ()
main = do
    evalFiles <- getArgs
    --print =<< readEval "UNH/UNH-benchmarkY1test.bm25.automatic.psg.eval.gz"
    evals <- mapM readEval evalFiles
    let grouped = M.unionsWith (++) [ M.singleton (runName, assess, metric) [score]
                                    | eval <- evals
                                    , (runName, Passage, assess, _query, metric, score) <- eval
                                    ]
    let stats = fmap (\xs -> (mean xs, stderr xs)) grouped
        runNames =
            sortBy (flip $ comparing $ \runName -> stats M.! (runName, Manual, RPrec))
            $ S.toList $ S.fromList [ runName | (runName, _, _) <- M.keys grouped ]

    let simpleHeader = Group SingleLine . map Header
        cols = (,) <$> [RPrec,MAP,RecipRank,NDCG] <*> [Manual,Automatic,Lenient]
        cells = [ [ s
                  | (metric,assess) <- cols
                  , let s = M.lookup (runName,assess,metric) stats
                  ]
                | runName <- runNames
                ]
    let table :: Table RunName (Metric, AssessmentMethod) (Maybe (Double, Double))
        table = Table (Group SingleLine (map Header runNames))
                      (Group SingleLine (map Header cols))
                      cells

    let showCell Nothing = "—"
        showCell (Just (m,s)) = showFFloat (Just 3) m . showString " ± " . showFFloat (Just 3) s $ ""
    --putStrLn $ render (escapeLatex . getRunName) show showCell table
    let pandocPage =  Pandoc.Pandoc mempty
            [toPandoc (textCell . getRunName) (textCell . show) (textCell . showCell) table]
    htmlText <- Text.Pandoc.Class.runIOorExplode $ Text.Pandoc.Writers.HTML.writeHtml5String Text.Pandoc.Options.def pandocPage
    putStrLn $ T.unpack htmlText
    --return ()

textCell :: String -> Pandoc.TableCell
textCell t = [Pandoc.Plain [Pandoc.Str t]]

toPandoc :: (rh -> Pandoc.TableCell) -> (ch -> Pandoc.TableCell) -> (a -> Pandoc.TableCell) -> Table rh ch a -> Pandoc.Block
toPandoc rowHeader colHeader cell (Table rows cols cells) =
    Pandoc.Table [] aligns widths colHeaders tableCells
  where
    nCols = length (headerContents cols) + 1
    aligns = replicate nCols Pandoc.AlignLeft
    widths = replicate nCols 0
    colHeaders = [] : map colHeader (headerContents cols)
    tableCells = [ rowHeader row : map cell rowCells
                 | (row, rowCells) <- zip (headerContents rows) cells
                 ]

escapeLatex :: String -> String
escapeLatex = foldMap f
  where f '_' = "\\_"
        f x   = [x]
