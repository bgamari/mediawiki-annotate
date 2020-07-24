{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}


module JointAspectFeatureData where

import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Control.Parallel.Strategies (NFData)
import qualified Data.Scientific
import qualified Data.Vector as Vector
import qualified Data.List
import qualified Codec.Compression.GZip as GZip
import Data.Scientific (toBoundedInteger, isInteger, Scientific, toRealFloat)

import GHC.Generics (Generic)
import Control.Monad ((<=<))

import SimplIR.Format.TrecRunFile as SimplirRun
import RankDataType 
import CAR.Types
import qualified Debug.Trace as Debug


type AspectId = PageId

type Feature = T.Text

data JointAspectFeatures = JointAspectFeatures { primary_link_example_id :: T.Text
                                                  , primary_link_example_to_compatability_feature_mapping :: M.Map Feature CompatibilityFeatureMapping
                                                  }
                                                  
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)


data CompatibilityFeatureMapping = CompatibilityFeatureMapping { compatability_feature_to_co_link_example_mapping :: M.Map PageId AspectMapping
                                                 }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

data AspectMapping = AspectMapping { co_aspect_to_primary_aspect_mapping :: M.Map AspectId CompatibilityMapping
                                                 }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

data CompatibilityMapping = CompatibilityMapping { compatability_mapping :: M.Map AspectId Scientific
                                                 }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)



readJordanJointAspectFormat :: FilePath -> IO [JointAspectFeatures]
readJordanJointAspectFormat filename = do
    bs <- fmap GZip.decompress $ BSL.readFile filename
    let decodeEntry :: BSL.ByteString -> IO (JointAspectFeatures)
        decodeEntry bs = either fail (return  ) 
                                $ Aeson.eitherDecode bs
    mapM decodeEntry (BSL.lines bs)




aspectToAspectRankData :: AspectId -> AspectId -> PageId -> RankData
aspectToAspectRankData a1 a2 e =
    RankData $ M.fromList [ (RankDataField "entity", RankDataText $ T.pack $ unpackPageId e)
                          , (RankDataField "aspect", RankDataList [T.pack $ unpackPageId a1, T.pack $ unpackPageId a2]) ]


convertToRunEntries :: JointAspectFeatures -> [(Feature, [SimplirRun.RankingEntry' T.Text RankData])]
convertToRunEntries (JointAspectFeatures {..} )=
    let queryId = primary_link_example_id
        entries =   [ (feature, 
                        [   SimplirRun.RankingEntry {
                                queryId = queryId
                            , documentName = aspectToAspectRankData a1 a2 entityId
                            , documentRank = 1
                            , documentScore = toRealFloat score 
                            , methodName = feature
                            } 
                        | (entityId, AspectMapping {..}) <- M.toList compatability_feature_to_co_link_example_mapping
                        , (a1, CompatibilityMapping{..}) <- M.toList co_aspect_to_primary_aspect_mapping
                        , (a2, score) <- M.toList compatability_mapping
                        , notSameEntity a1 a2
                        , (Just 0) /=  toBoundedInteger @Int score
                        ])
                    | (feature, CompatibilityFeatureMapping {..}) <- M.toList primary_link_example_to_compatability_feature_mapping
                    ]
    in entries                
  where notSameEntity a1 a2 =
          let (e1, _) = T.span (/= '/') $ T.pack $ unpackPageId a1  
              (e2, _) = T.span (/= '/') $ T.pack $ unpackPageId a2
          in e1 /= e2