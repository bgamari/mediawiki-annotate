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




opts :: Parser ( [FilePath]
               , [FilePath]
               , [T.Text]
               )
opts =
    (,,)
    <$> many (option str (long "eval1" <> metavar "Eval-FILE"))
    <*> many (option str (long "eval2" <> metavar "Eval-FILE"))
    <*> many (option str (short 'm' <> long "metric" <> metavar "METRIC" <> help "eval metric to use, one of map recip_rank Rprec"))


main :: IO ()
main = do
    hSetBuffering stdout LineBuffering

    (eval1Globs, eval2Globs, metrics) <- execParser' 1 (helper <*> opts) mempty
    eval1 <- concat <$> mapM glob eval1Globs
    eval2 <- concat <$> mapM glob eval2Globs
    eval1Runs <-  mapM (TrecEvalFile.readEvalFile metrics) eval1
    eval2Runs <-  mapM (TrecEvalFile.readEvalFile metrics) eval2


    let experiment :: (EvalEntry -> Double) -> IO()
        experiment evalMetric = do
          let systemRanking1 = sortedList evalMetric evalRunId eval1Runs
              systemRanking2 = sortedList evalMetric evalRunId eval2Runs

              universe1 = HS.fromList systemRanking1
              universe2 = HS.fromList systemRanking2

          when (universe1 /= universe2) $ fail ( "Sets are not identical: " ++ "("++ show universe1 ++ "  /   "++ show universe2 ++ ")")

          let concordantSet = HS.size $ (sortedPairs systemRanking1) `HS.intersection` (sortedPairs systemRanking2)
              allSet = n * ( n -1 ) `div` 2
                where n = HS.size universe1
              discordantSet = allSet - concordantSet

              kendallsTau :: Double
              kendallsTau = realToFrac (concordantSet - discordantSet) / realToFrac (concordantSet + discordantSet)

          putStrLn $ "Concordant = " ++ show (concordantSet)
          putStrLn $ "Discordant = " ++ show (discordantSet)
          putStrLn $ "Universe Size = " ++ show (HS.size universe1)
          putStrLn $ "Kendalls Tau = " ++ show kendallsTau


          let rankDiffsSq = [ (r1-r2)^2
                            | (r1, s1)<- zip [1.. ] systemRanking1
                            , (r2, s2)<- zip [1.. ] systemRanking2
                            , s1 == s2
                            ]
              n = realToFrac $ length systemRanking2
              spearmansRank = 1.0 - 6.0 * realToFrac (sum rankDiffsSq) / (n * (n^2 -1.0))

          putStrLn $ "Spearman's Rank Correlation = " ++ show spearmansRank

    forM_ metrics (\metric -> do
        putStrLn $ "\n==========="
        T.putStrLn $ metric
        let fromJust' = fromMaybe (error $ "Metric "<>T.unpack metric<>" not defined")
        experiment (\ee -> fromJust' $ metric `HM.lookup` (evals ee))
      )


sortedList :: forall ee identifier . (ee -> Double) -> (ee -> identifier)  -> [ee] -> [identifier]
sortedList scoreFun idFun evalRuns =
        let eval1Sorted :: [identifier]
            eval1Sorted = fmap snd
                   $ Ranking.toSortedList
                   $ Ranking.fromList
                   $ [ (score, idFun ee)
                     | ee <- evalRuns
                     , let !score = scoreFun ee
                     ]

        in eval1Sorted

--
-- sortedListDebug :: forall ee identifier . (Show identifier) =>  (ee -> Double) -> (ee -> identifier)  -> [ee] ->IO ()
-- sortedListDebug scoreFun idFun eval1Runs = do
--         let sortedAll =
--                        Ranking.toSortedList
--                        $ Ranking.fromList
--                        $ [ (score, idFun ee)
--                          | ee <- eval1Runs
--                          , let score = scoreFun ee
--                          ]
--
--             eval1Sorted :: [identifier]
--             eval1Sorted = fmap snd sortedAll
--
--         print sortedAll

sortedPairs :: (Eq identifier, Hashable identifier) => [identifier] -> HS.HashSet (identifier, identifier)
sortedPairs evalSorted = HS.fromList [ (upper,lower)
                                      | upper : rest <- tails evalSorted
                                      , lower <- rest
                                      ]
