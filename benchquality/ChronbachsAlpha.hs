{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

import Control.Monad
import Options.Applicative
import System.IO

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List
import Data.Maybe
import Data.Hashable

import System.FilePath.Glob

import qualified SimplIR.Ranking as Ranking
import CAR.ToolVersion
import TrecEvalFile

type Metric = T.Text


opts :: Parser ( [FilePath]
               , T.Text
               )
opts =
    (,)
    <$> many (option str (long "eval" <> metavar "Eval-FILE"))
    <*> (option str (short 'm' <> long "metric" <> metavar "METRIC" <> help "eval metric to use, one of map recip_rank Rprec"))


main :: IO ()
main = do
    hSetBuffering stdout LineBuffering

    (eval1Globs, metric) <- execParser' 1 (helper <*> opts) mempty
    eval1 <- concat <$> mapM glob eval1Globs
--     eval1Runs <-  mapM (TrecEvalFile.readQueryEvalFile metric) eval1

    evalSet <- mconcat <$> mapM (readQueryEvalFile metric) eval1

    putStrLn $ "Chronbach's Alpha = " ++ show (chronbachAlpha metric evalSet)



chronbachAlpha ::  Metric -> [QueryEvalEntry] -> Double
chronbachAlpha  targetMetric evalSet =
    let queryToEvals = HM.fromListWith (<>) [(queryField, [qee]) | qee@(QueryEvalEntry fname queryField evalScore) <- evalSet]
        systemToEvals = HM.fromListWith (<>) [(fname, [qee]) | qee@(QueryEvalEntry fname queryField evalScore) <- evalSet]
        sumOfItemVariances = sum
                           [ variance (fmap qevalScore qees)
                           | (_, qees) <- HM.toList queryToEvals
                           ]
        varianceOfStudentTotals = variance
                                [ sum (fmap qevalScore qees)
                                | (_, qees) <- HM.toList systemToEvals
                                ]
        k = realToFrac $ HM.size queryToEvals
        alpha = k / (k-1) * (1 - sumOfItemVariances / varianceOfStudentTotals)
    in alpha

  where variance scores =
            let mu = mean scores
            in mean [ (mu - s)^2 | s <- scores]
        mean scores = sum scores / realToFrac (length scores)