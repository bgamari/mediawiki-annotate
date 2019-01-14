{-# LANGUAGE DataKinds #-}
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
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}


module AspectUtils where

import qualified Data.Text as T
import Data.List
import Data.Maybe
import Data.Foldable as Foldable

-- import Data.Hashable--import Data.List.Split

import CAR.Types hiding (Entity)
import qualified CAR.RunFile as CarRun
import qualified SimplIR.Format.TrecRunFile  as Run
type AspectId = (PageId, HeadingId)

-- --------------------------
--    TrecRunFile for Aspect Runs
-- --------------------------

type AspectRankingEntry = CarRun.RankingEntry' AspectId

parseAspectId :: T.Text -> [AspectId]
parseAspectId s =
    case T.split (== '/') s of
        []          -> []
        pid:[]      -> []
        pid:hs:[]   -> [makeId pid hs]
        splits      ->  let positions :: [Int]
                            positions = [ i
                                        | (s, i) <- (T.unpack s) `zip` [1..]
                                        , s == '/'
                                        ]
                        in [ makeId pref suffix
                           | pos <- positions
                           , let (pref,slashSuffix) = T.splitAt pos s
                                 suffix = T.drop 1 slashSuffix
                           ]
  where makeId :: T.Text -> T.Text -> AspectId
        makeId pid hs = ((packPageId $ T.unpack pid), (packHeadingId $ T.unpack hs))

parseAspectId' :: T.Text -> AspectId
parseAspectId' s =
    case T.split (== '/') s of
        []          -> error "[]"
        pid:[]      -> error "pid:[]"
        pid:hs:[]   -> makeId pid hs
        splits      ->  let positions :: [Int]
                            positions = [ i
                                        | (s, i) <- (T.unpack s) `zip` [1..]
                                        , s == '/'
                                        ]
                        in head $
                           [ makeId pref suffix
                           | pos <- positions
                           , let (pref,slashSuffix) = T.splitAt pos s
                                 suffix = T.drop 1 slashSuffix
                           ]
  where makeId :: T.Text -> T.Text -> AspectId
        makeId pid hs = ((packPageId $ T.unpack pid), (packHeadingId $ T.unpack hs))


readAspectRun :: FilePath -> IO [AspectRankingEntry]
readAspectRun path = map (CarRun.toCarRankingEntry parseDoc) <$> Run.readRunFile path
  where parseDoc :: (Run.DocumentName -> AspectId)
        parseDoc = parseAspectId'

