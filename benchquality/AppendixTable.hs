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



data RunType = Entity | Passage
             deriving (Eq, Ord, Show)
newtype RunName = RunName { getRunName :: String }
                deriving (Eq, Ord, Show)

newtype ColHeader = ColHeader { getHeader :: String }
                deriving (Eq, Ord, Show)
newtype Query = Query BS.ByteString
              deriving (Show)


type Metric = String
type AssessmentMethod = String

parseRunName name =
    case name of
      "Entity" -> Entity
      "Passage" -> Passage
      s -> error ("unknown runName "<> show s)

opts :: Parser ([FilePath], FilePath, [Metric], Metric, [AssessmentMethod], AssessmentMethod, RunType, String)
opts =
    (,,,,,,,)
    <$> some (argument str (metavar "evalfiles" <> help "A glob pattern for evalfiles"))
    <*> option str (short 'o' <> long "out" <> help "output file")
    <*> some (option str (short 'm' <> long "metric" <> help "evaluation metric to include, (e.g. Rprec, map, ndcg_cut_5)"))
    <*> option str (short 'M' <> long "sort-metric" <> help "evaluation metric to sort results by")
    <*> some (option str (short 'a' <> long "assessment" <> help "assessment method to include (e.g. manual, automatic, lenient)"))
    <*> option str (short 'A' <> long "sort-assessment" <> help "assessment method to sort results by")
    <*> option (parseRunName <$> str) (short 'r' <> long "run-type" <> help "Passage or Entity")
    <*> option str (short 't' <> long "title" <> help "page title. will be appended with \"Entity/Passage task\"")

-- Expect file names to be of this format:
--  $methodname.$assess.$runType.eval*
-- example: mpii-nn6_pos.automatic.psg.eval.gz

readEval ::  S.Set Metric-> S.Set AssessmentMethod -> FilePath -> IO [(RunName, RunType, AssessmentMethod, Query, Metric, Double)]
readEval metrics assessmentMethods path  =
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
                        s     -> error $ "unknown runType "++ show s
            assess = parts !! 1

        in mapMaybe (parseLine . BSL.split '\t') $ BSL.lines x

    parseMetric name =
        if name `S.member` metrics then
            Just name
        else
            Nothing

mean :: RealFrac a => [a] -> a
mean xs = sum xs / realToFrac (length xs)

stddev :: RealFloat a => [a] -> a
stddev xs = sqrt $ mean [ (x - m)^2 | x <- xs ]
  where m = mean xs

stderr :: RealFloat a => [a] -> a
stderr xs = stddev xs / sqrt (realToFrac $ length xs)

main :: IO ()
main = do
    (evalGlobs, output, metrics, sortMetric, assessmentMethods, sortAssessmentMethod, requestedRunType, title) <- execParser $ info (helper <*> opts) mempty
    evalFiles <- concat <$> mapM glob evalGlobs
    let assessmentMethodsSet = S.fromList assessmentMethods
        metricsSet = S.fromList metrics
    evals <- mapM (readEval metricsSet assessmentMethodsSet) evalFiles
    let groupedData = M.unionsWith (++) [ M.singleton (runName, assess, metric) [score]
                                    | eval <- evals
                                    , (runName, runType, assess, _query, metric, score) <- eval
                                    , assess `S.member` assessmentMethodsSet
                                    , metric `S.member` metricsSet
                                    , runType ==  requestedRunType
                                    ]
    let stats = fmap (\xs -> (mean xs, stderr xs)) groupedData
        runNames =
            sortBy (flip $ comparing $ \runName -> stats M.! (runName, sortAssessmentMethod, sortMetric))
            $ S.toList $ S.fromList [ runName | (runName, _, _) <- M.keys groupedData ]

    let simpleHeader = Group SingleLine . map Header
        cols = [(m,a) |  a <- assessmentMethods, m <- metrics]
        cells = [ [ s
                  | (metric,assess) <- cols
                  , let s = M.lookup (runName,assess,metric) stats
                  ]
                | runName <- runNames
                ]
    let table :: Table RunName ColHeader (Maybe (Double, Double))
        table = Table (Group SingleLine (map Header runNames))
                      (Group SingleLine (map Header (fmap prettyCols cols)))
                      cells



    let showCell Nothing = "—"
        showCell (Just (m,s)) = showFFloat (Just 3) m . showString " ± " . showFFloat (Just 3) s $ ""

        createPandocTable table =
            [toPandoc (textCell . getRunName) (textCell . getHeader) (textCell . showCell) table]

        pandocPage =  Pandoc.Pandoc mempty (
                        [pandocHeader (title <> " " <> show requestedRunType <> " Task")]
                        ++ (createPandocTable table))


    let htmlOpts = Text.Pandoc.Options.def {Text.Pandoc.Options.writerPreferAscii = True}
    htmlText <- Text.Pandoc.Class.runIOorExplode $ Text.Pandoc.Writers.HTML.writeHtml5String htmlOpts pandocPage
    writeFile output $ T.unpack htmlText
    return ()

prettyCols :: (Metric, AssessmentMethod) -> ColHeader
prettyCols (metric, assessment) = ColHeader $ metric <> "/" <> assessment

pandocHeader :: String -> Pandoc.Block
pandocHeader text = Pandoc.Header 1 mempty [Pandoc.Str text, Pandoc.Space ]

textCell :: String -> Pandoc.TableCell
textCell t = [Pandoc.Plain [Pandoc.Str t, Pandoc.Space]]

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
