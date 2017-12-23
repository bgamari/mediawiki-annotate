{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module CAR.Types.Provenance where

import GHC.Generics
import Data.String

import qualified Data.Text as T
import qualified Data.Aeson as A
import qualified Codec.CBOR.JSON as CBOR.JSON
import qualified Codec.Serialise          as CBOR
import Data.Hashable

import CAR.Types.AST

-- | An IETF (RFC 5646, RFC 4647) language code
newtype Language = Language T.Text
                 deriving (Eq, Ord, Show, Hashable, IsString, CBOR.Serialise, Generic)

data SiteProvenance = SiteProvenance { provSiteId   :: SiteId
                                     , language     :: Language
                                     , sourceName   :: T.Text
                                     , siteComments :: [T.Text]
                                     }
                    deriving (Eq, Show, Generic)
instance CBOR.Serialise SiteProvenance 

data Transform = Transform { toolName   :: T.Text
                           , toolCommit :: T.Text
                           , toolInfo   :: JSON
                           }
               deriving (Eq, Show, Generic)

instance CBOR.Serialise Transform
instance A.ToJSON Transform
instance A.FromJSON Transform

transform :: A.ToJSON a => T.Text -> T.Text -> a -> Transform
transform toolName toolCommit x =
    Transform { toolInfo = JSON $ A.toJSON x, ..}

newtype JSON = JSON A.Value
             deriving (Eq, Show, A.ToJSON, A.FromJSON)

instance CBOR.Serialise JSON where
    decode = JSON <$> CBOR.JSON.decodeValue False
    encode (JSON v) = CBOR.JSON.encodeValue v

data Provenance = Provenance { siteProvenances :: [SiteProvenance]
                             , dataReleaseName :: T.Text
                             , comments        :: [T.Text]
                             , transforms      :: [Transform]
                             }
                deriving (Eq, Show, Generic)
instance CBOR.Serialise Provenance

-- | A backwards compatibility shim.
wikiSite :: Provenance -> SiteId
wikiSite (Provenance { siteProvenances = sites })
  | [] <- sites  = error "wikiSite: No sites"
  | [s] <- sites = provSiteId s
  | otherwise    = error "wikiSite: Too many sites"
{-# DEPRECATED wikiSite "This shalln't be used" #-}

invalidProvenance :: Provenance
invalidProvenance = Provenance [] "invalid" [] []