{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics
import Data.Aeson
import Data.Hashable
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Codec.Serialise as CBOR

import qualified Data.JsonStream.Parser as JS

newtype Lang = Lang T.Text
             deriving (Show, Eq, Ord, Hashable, FromJSON, FromJSONKey, CBOR.Serialise)

newtype ItemId = ItemId Int
                 deriving (Show, Eq, Ord, Hashable, CBOR.Serialise)

instance FromJSON ItemId where
    parseJSON = withText "item id" $ maybe (fail "invalid item id") pure . readItemId

readItemId :: T.Text -> Maybe ItemId
readItemId s
  | Just rest <- T.stripPrefix "Q" $ T.strip s
  , Right (n, _) <- TR.decimal rest
  = Just $ ItemId n
  | otherwise
  = Nothing

newtype SiteId = SiteId T.Text
               deriving (Show, Eq, Ord, Hashable, FromJSON, ToJSON, FromJSONKey, CBOR.Serialise)

newtype PageName = PageName T.Text
               deriving (Show, Eq, Ord, Hashable, FromJSON, ToJSON, CBOR.Serialise)

data EntityType = Item
                deriving (Show, Generic)
instance CBOR.Serialise EntityType
instance FromJSON EntityType where
    parseJSON = withText "entity type" $ \s ->
      case s of
        "item" -> return Item
        _      -> fail "unknown entity type"

data Entity = Entity { entityType :: EntityType
                     , entityId   :: ItemId
                     , entityLabels :: [(Lang, T.Text)]
                     , entitySiteLinks :: [(SiteId, PageName)]
                     }
            deriving (Show, Generic)
instance CBOR.Serialise Entity

instance FromJSON Entity where
    parseJSON = withObject "entity" $ \o ->
        Entity <$> o .: "type"
               <*> o .: "id"
               <*> (o .: "labels" >>= withObject "labels" parseLabels)
               <*> (o .: "sitelinks" >>= withObject "site links" parseSitelinks)
      where
        parseLabels = mapM parseLabel . HM.elems
        parseLabel = withObject "label" $ \o ->
              (,) <$> o .: "language"
                  <*> o .: "value"

        parseSitelinks = mapM parseSitelink . HM.elems
        parseSitelink = withObject "site link" $ \o ->
              (,) <$> o .: "site"
                  <*> o .: "title"

buildIndex :: BSL.ByteString -> HM.HashMap ItemId (HM.HashMap SiteId PageName)
buildIndex =
    foldMap f . JS.parseLazyByteString (JS.arrayOf (JS.value @Entity))
  where
    f :: Entity -> HM.HashMap ItemId (HM.HashMap SiteId PageName)
    f e
      | null (entitySiteLinks e) = mempty
      | otherwise                = HM.singleton (entityId e) (HM.fromList $ entitySiteLinks e)

main :: IO ()
main = do
    --BSL.getContents >>= print . eitherDecode @[Entity]
    BSL.getContents >>= pure . buildIndex >>= BSL.writeFile "out" . CBOR.serialise
