{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CAR.Types.Orphans () where

import Data.Aeson.Types
import Control.DeepSeq
import qualified Codec.Serialise.Class as CBOR
import qualified Data.ByteString.Short as SBS

import Data.MediaWiki.Markup

deriving instance CBOR.Serialise PageName
deriving instance FromJSON PageName
instance NFData PageName
instance FromJSONKey PageName where
    fromJSONKey = fmap PageName fromJSONKey
deriving instance ToJSON PageName
instance ToJSONKey PageName where
    toJSONKey = contramapToJSONKeyFunction (\(PageName n) -> n) toJSONKey

instance CBOR.Serialise SBS.ShortByteString where
    encode = CBOR.encode . SBS.fromShort
    decode = SBS.toShort <$> CBOR.decode   -- FIXME: copy
