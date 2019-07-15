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
import GHC.Stack
import GHC.Generics
import Codec.Serialise
import Control.DeepSeq
import Data.Hashable
-- import Control.Concurrent.Async
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty ( NonEmpty ((:|)))



-- import Data.Hashable--import Data.List.Split

import CAR.Types hiding (Entity)
import qualified Clone.RunFile as CarRun
import qualified SimplIR.Format.TrecRunFile  as Run
data AspectId = AspectFromTuple PageId HeadingId
              | AspectFromString T.Text
              deriving (Show, Generic, Serialise)
instance NFData AspectId


-- | Not intended to be used. Internal string representation used to eq/compare AspectIds of different constructors
internalAspectStr :: AspectId -> String
internalAspectStr (AspectFromTuple p h) =
    ((unpackPageId p) <> "/" <> (unpackHeadingId h) )
internalAspectStr (AspectFromString idStr) =  T.unpack $ idStr

instance Eq AspectId where
    aspect1@(AspectFromTuple p h) == aspect2@(AspectFromString _) =
        internalAspectStr aspect1 == internalAspectStr aspect2
    aspect2@(AspectFromString _) == aspect1@(AspectFromTuple p h) =
        internalAspectStr aspect1 == internalAspectStr aspect2
    (AspectFromTuple p1 h1) == (AspectFromTuple p2 h2) =
        p1 == p2 && h1 == h2
    (AspectFromString idStr1) == (AspectFromString idStr2) =
        idStr1 == idStr2

instance Ord AspectId where
    compare aspect1 aspect2 =
        compare (internalAspectStr aspect1) (internalAspectStr aspect2)

instance Hashable AspectId where
    hashWithSalt salt aspect =
        hashWithSalt salt $ internalAspectStr aspect

--  | Constructor: make aspect from tuple
makeAspectId :: PageId -> HeadingId -> AspectId
makeAspectId p h = AspectFromTuple p h

-- | Constructor: make aspect from String
parseAspectId :: T.Text -> AspectId
parseAspectId s =
    AspectFromString s


-- | pageId check
aspectHasPageId :: PageId -> AspectId -> Bool
aspectHasPageId pageId1 (AspectFromTuple pageId2 _) = pageId1 == pageId2
aspectHasPageId pageId1 (AspectFromString idStr) =
    let pageId1S = T.pack $ ((unpackPageId pageId1) <> "/" )
    in  pageId1S `T.isPrefixOf` idStr


-- | pageId and headingId check
aspectMatchPageAndHeadingId :: PageId -> HeadingId -> AspectId -> Bool
aspectMatchPageAndHeadingId pageId1 headingId1 (AspectFromTuple pageId2 headingId2) =
    pageId1 == pageId2 && headingId1 == headingId2
aspectMatchPageAndHeadingId pageId1 headingId1 (AspectFromString idStr) =
    internalAspectStr (AspectFromTuple pageId1 headingId1) == T.unpack idStr


-- | provide all valid (potential) page ids -- at least one!
aspectValidPageIds :: AspectId -> NE.NonEmpty PageId
aspectValidPageIds (AspectFromTuple pageId _) = pageId :| []
aspectValidPageIds (AspectFromString idStr) =
    let validPageIdStrings = inits $ T.splitOn "/" idStr
    in NE.fromList
       $ fmap (packPageId . T.unpack . (T.intercalate "/")) validPageIdStrings


-- | provide the matching heading id (for a valid page id)
getAspectHeadingId :: AspectId -> PageId -> Maybe HeadingId
getAspectHeadingId (AspectFromTuple p h) p2 =
    if p == p2 then
        Just h
    else
        Nothing
getAspectHeadingId (AspectFromString idStr) p2 =
    if p2Str `T.isPrefixOf` idStr then
        if T.length idStr > (T.length p2Str) +1 then
            let hStr = T.drop ((T.length p2Str)+1) idStr
            in Just $ packHeadingId $ T.unpack hStr
        else
            Nothing -- not enough string left for a heading
    else
        Nothing -- does not match given PageId
  where p2Str = T.pack $ unpackPageId p2


-- --------------------------
--    TrecRunFile for Aspect Runs
-- --------------------------

type AspectRankingEntry = CarRun.RankingEntry' AspectId


readAspectRun :: FilePath -> IO [AspectRankingEntry]
readAspectRun path = map (CarRun.toCarRankingEntry parseDoc) <$> Run.readRunFile path
  where parseDoc :: (Run.DocumentName -> AspectId)
        parseDoc = parseAspectId

head' :: HasCallStack => [a] -> a
head' (x:_) = x
head' [] = error $ "head': empty list"