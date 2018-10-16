{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A simple parser for the TREC eval file format for ranking evaluation (output of trec_eval)

module TrecEvalFile
  ( EvalEntry(..)
  , readEvalFile
  , QueryEvalEntry(..)
  , readQueryEvalFile
  ) where

import Data.Semigroup
import Data.Maybe
import Control.Monad
import Control.Applicative

import Data.Maybe
import Data.List
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import qualified Data.Text.Lazy.Builder.RealFloat as TB
import qualified Data.Text.Lazy.IO as TL
import qualified Data.HashMap.Strict as HM
import Text.Trifecta

type Score = Double
type MethodName = T.Text
type QueryField = T.Text
type Metric = T.Text


data EvalEntry = EvalEntry { evalRunName   :: !FilePath
                           , evalRunId     :: !MethodName
                           , evals         :: !(HM.HashMap T.Text Score)
                           }
                  deriving (Show)
data QueryEvalEntry = QueryEvalEntry { qevalRunName   :: !FilePath
                                     , qqueryField    :: !QueryField
                                     , qevalScore     :: !Score
                                     }
                  deriving (Show)
data EvalLine = EvalLine { metric   :: !T.Text
                         , queryField      :: !QueryField
                         , evalScore       :: !(Either Score MethodName)
                         }
                  deriving (Show)

parseEvalLine :: Parser [EvalLine]
parseEvalLine = runUnlined (catMaybes <$> many (fmap Just parseLine <|> whitespaceOnly))
  where whitespaceOnly = spaces >> newline >> pure Nothing

-- one entry looks like this:
-- lucene-luceneindex-ecm-pages.cbor-unprocessedAllButBenchmark.v201.cbor.lucene-ecm--entity-section--all-ql-ecm--Text-benchmarkY1train.v201.cbor.outlines.run
--   runid                   all     myFunkyMethodname
--   map                     all     0.0247
--   Rprec                   all     0.0337
--   recip_rank              all     0.0904
-- where all can also be replaces by a query id

parseLine :: Unlined Parser EvalLine
parseLine = do
    void $ many newline
    let textField = fmap T.pack $ some $ noneOf " \t\n"
    metric <- textField
    void spaces
    queryField <- textField  -- reserved (Q1?)
    void spaces
    evalScore <- fmap (Left . either realToFrac id) (try integerOrDouble) <|>  fmap Right textField
    --void space
    void (some newline) <|> eof
    return $ EvalLine {..}

readEvalFile :: [Metric ]-> FilePath ->  IO EvalEntry
readEvalFile metrics fname = do
    mbEvalLines <- parseFromFile parseEvalLine fname
    evalLines <- maybe (fail "Failed to parse run file") pure mbEvalLines
    let evalMap :: HM.HashMap Metric (Either Score MethodName)
        evalMap  =  HM.fromList
                 $  [ (metric, evalScore)
                    | EvalLine{..} <- evalLines
                    , queryField == "all"
                    , metric `elem` (metrics ++ ["runid"])
                    ]

        fetchedEvals = HM.fromList [ (m, score)
                                   | m <- metrics
                                   , Just (Left score) <- pure $ m `HM.lookup` evalMap
                                   ]
        Just (Right runId) = HM.lookup "runid" evalMap
    return EvalEntry { evalRunName = fname
                     , evalRunId = runId
                     , evals = fetchedEvals
                     }

readQueryEvalFile ::  Metric -> FilePath -> IO [QueryEvalEntry]
readQueryEvalFile targetMetric fname = do
    mbEvalLines <- parseFromFile parseEvalLine fname
    evalLines <- maybe (fail "Failed to parse run file") pure mbEvalLines

    return      [ (QueryEvalEntry fname queryField evalScore')
                | EvalLine{..} <- evalLines
                , queryField /= "all"
                , metric == targetMetric
                , Left evalScore' <- pure evalScore
                ]
