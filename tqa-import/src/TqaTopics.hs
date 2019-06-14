{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module TqaTopics where

import GHC.Generics

import qualified Data.Text as T
import Data.Aeson
import Data.Hashable
import Data.Set as S

import CAR.Types

-- import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M


type SectionPathId = [SectionPathElem]

data SectionPathElem = SectionPathPage PageId  | SectionPathHeading HeadingId
  deriving (Eq, Ord, Show, Generic, ToJSONKey, ToJSON, FromJSONKey, FromJSON)


data TqaStatus = TqaStatus { titles :: M.Map PageId T.Text
                             , headings :: M.Map SectionPathId T.Text
                             , notes :: M.Map SectionPathId T.Text
                             , includePages :: S.Set PageId
                             , includeSections :: S.Set SectionPathId
                             , trainTitles :: S.Set PageId
                            }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

emptyTqaStatus = TqaStatus mempty mempty mempty mempty mempty mempty
