{-# LANGUAGE DeriveGeneric #-}

module Types where

import GHC.Generics
import qualified Data.Binary.Serialise.CBOR as CBOR
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
