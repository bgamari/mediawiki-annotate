{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DeriveGeneric #-}

module WikiData where

import CAR.Types (PageName)
import GHC.Generics
import Data.Aeson
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Codec.Serialise as CBOR

type LangIndex = HM.HashMap ItemId (HM.HashMap SiteId PageName)

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

-- SiteId enwiki or itwiki, envoyage
newtype SiteId = SiteId T.Text
               deriving (Show, Eq, Ord, Hashable, FromJSON, ToJSON, FromJSONKey, CBOR.Serialise)
               
data EntityType = Item
                deriving (Show, Generic)
instance CBOR.Serialise EntityType
instance FromJSON EntityType where
    parseJSON = withText "entity type" $ \s ->
      case s of
        "item" -> return Item
        _      -> fail "unknown entity type"

-- | A projection of the wikidata entity representation.
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


createLookup :: LangIndex -> SiteId -> SiteId -> HM.HashMap PageName PageName
createLookup index fromLang toLang =
    HM.fromList
    [ (fromPage, toPage )
    | entries <- HM.elems index
    , Just fromPage <- pure $ fromLang `HM.lookup` entries
    , Just toPage <- pure $ toLang `HM.lookup` entries
    ]


loadLangIndex :: FilePath -> IO LangIndex
loadLangIndex =
    CBOR.readFileDeserialise
