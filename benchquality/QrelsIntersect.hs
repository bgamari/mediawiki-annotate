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
import GHC.Generics
import Codec.Serialise

import qualified Data.Map.Strict as M
import qualified Data.Map.Lazy as ML
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text.IO as T
import qualified Data.Vector.Indexed as VI
import Data.List
import Data.Maybe
import Data.Foldable as Foldable
import Data.Function
import Data.Bifunctor
import Data.Hashable

import System.FilePath


import qualified SimplIR.Format.QRel as QRel
import SimplIR.Format.QRel
import qualified SimplIR.Ranking as Ranking
import CAR.ToolVersion

import Control.Monad (when)
import GHC.Generics
import Data.Semigroup hiding (option)

import Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as VU
import Options.Applicative
import System.Random



opts :: Parser ( FilePath
               , FilePath
               , Int
               )
opts =
    (,,)
    <$> option str (long "qrels1" <> metavar "Qrel-FILE")
    <*> option str (long "qrels2" <> metavar "QREL-FILE")
    <*> option auto (long  "relthresh" <> metavar "REL" <> help "lowest threshold for relevant qrel entries, default: 1" <> value 1)


main :: IO ()
main = do
    hSetBuffering stdout LineBuffering

    (qrel1, qrel2, relthresh) <- execParser' 1 (helper <*> opts) mempty

    let fixQRel (Entry qid docId rel) =
          QRel.Entry qid docId rel'
            where rel' :: QRel.IsRelevant
                  rel' = if (QRel.unGradedRelevance rel) < relthresh
                            then QRel.NotRelevant
                            else QRel.Relevant

    qrel1 <- map fixQRel <$> QRel.readQRel @QRel.GradedRelevance qrel1
    qrel2<- map fixQRel <$> QRel.readQRel @QRel.GradedRelevance qrel2

    let qrel1Map = HM.fromListWith (<>) [ (qid, [docid])
                                      | Entry qid docid Relevant <- qrel1
                                      ]
        qrel2Map = HM.fromListWith (<>) [ (qid, [docid])
                                      | Entry qid docid Relevant <- qrel2
                                      ]

        statsdata = [  HS.size ( doc1set  `HS.intersection` doc2set)
                    | (qid, docids1) <- HM.toList qrel1Map
                    , let docids2 = fromMaybe [] $ qid `HM.lookup` qrel2Map
                    , let doc1set = HS.fromList docids1
                    , let doc2set = HS.fromList docids2
                    ]

        total = sum statsdata

        numqueries = min (HM.size qrel1Map) (HM.size qrel2Map)

    putStrLn $ "numqueries = "++show numqueries
    putStrLn $ "totalshared = " ++ show total
    putStrLn $ "avgshared =" ++ show ((realToFrac total) / (realToFrac numqueries))

