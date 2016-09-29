{-# LANGUAGE DeriveGeneric #-}

module Types where

import GHC.Generics
import qualified Data.Binary.Serialise.CBOR as CBOR
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Data.MediaWiki.Markup

instance CBOR.Serialise PageName

data PageSkeleton = Section !T.Text [PageSkeleton]
                  | Para ![ParaBody]
                  deriving (Show, Generic)
instance CBOR.Serialise PageSkeleton

data ParaBody = ParaText !T.Text
              | ParaLink !PageName !T.Text
              deriving (Show, Generic)
instance CBOR.Serialise ParaBody

data Page = Page { pageName :: PageName, pageSkeleton :: [PageSkeleton] }
          deriving (Show, Generic)
instance CBOR.Serialise Page

readValues :: CBOR.Serialise a => BSL.ByteString -> [a]
readValues = start . BSL.toChunks
  where
    start = go CBOR.deserialiseIncremental
    go _ [] = error "ran out of data"
    go (CBOR.Partial f) (bs : bss) = go (f (Just bs)) bss
    go (CBOR.Done bs _ x) bss = x : start (bs : bss)
    go (CBOR.Fail rest _ err) _ = error $ show err
