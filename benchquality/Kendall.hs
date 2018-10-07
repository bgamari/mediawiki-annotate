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

import Control.Concurrent.Async
import Control.Monad
import Data.Ord
import Data.Tuple
import Data.Semigroup hiding (All, Any, option)
import Options.Applicative
import System.IO
import Data.Aeson
import Numeric.Log
import GHC.Generics
import Codec.Serialise

import qualified Data.Map.Strict as M
import qualified Data.Map.Lazy as ML
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector.Indexed as VI
import qualified Data.Vector.Unboxed as VU
import Data.List
import Data.Maybe
import Data.Foldable as Foldable
import Data.Function
import Data.Bifunctor
import Data.Hashable

import System.FilePath
import System.FilePath.Glob

import qualified CAR.RunFile as CAR.RunFile
import qualified SimplIR.Format.QRel as QRel
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

--     putStrLn "--------"
--     putStrLn "eval1Runs"
--     sortedListDebug evalMap evalRunId eval1Runs
--     putStrLn "--------"
--     putStrLn "eval2Runs"
--     sortedListDebug evalMap evalRunId eval2Runs


    let experiment evalMetric = do
          let eval1Set = sortedList evalMetric evalRunId eval1Runs
              eval2Set = sortedList evalMetric evalRunId eval2Runs

              universe1 = HS.fromList eval1Set
              universe2 = HS.fromList eval2Set

          when (universe1 /= universe2) $ fail ( "Sets are not identical: " ++ "("++ show universe1 ++ "  /   "++ show universe2 ++ ")")

          let concordantSet = HS.size $ (sortedPairs eval1Set) `HS.intersection` (sortedPairs eval2Set)
              allSet = n * ( n -1 ) `div` 2
                where n = HS.size universe1
              discordantSet = allSet - concordantSet

              kendallsTau = realToFrac (concordantSet - discordantSet) / realToFrac (concordantSet + discordantSet)


      --     , let method = takeFileName evalRunName

          putStrLn $ "Concordant = " ++ show (concordantSet)
          putStrLn $ "Discordant = " ++ show (discordantSet)
          putStrLn $ "Universe Size = " ++ show (HS.size universe1)
          putStrLn $ "Kendalls Tau = " ++ show kendallsTau

    forM_ metrics (\metric -> do
        putStrLn $ "\n==========="
        T.putStrLn $ metric
        let fromJust' = fromMaybe (error $ "Metric "<>T.unpack metric<>" not defined")
        experiment (\ee -> fromJust' $ metric `HM.lookup` (evals ee))
      )


sortedList :: forall ee identifier . (ee -> Double) -> (ee -> identifier)  -> [ee] -> [identifier]
sortedList scoreFun idFun eval1Runs =
        let eval1Sorted :: [identifier]
            eval1Sorted = fmap snd
                   $ Ranking.toSortedList
                   $ Ranking.fromList
                   $ [ (score, idFun ee)
                     | ee <- eval1Runs
                     , let score = scoreFun ee
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
