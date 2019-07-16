{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module CAR.Types.Orphans () where

-- import Data.Aeson.Types
-- import Control.DeepSeq
-- import qualified Codec.Serialise.Class as CBOR
-- import qualified Data.ByteString.Short as SBS
-- import Data.Hashable
-- import qualified Data.Text as T
-- import Data.String (IsString)
-- import GHC.Generics
--
-- -- import Text.Parsers.Frisby  hiding ((<>))
-- -- import Text.Parsers.Frisby.Char
--
-- import Data.Char
--
-- -- import Data.MediaWiki.Markup
--
-- newtype PageName = PageName { getPageName :: T.Text }
--                  deriving (Show, Generic, IsString)
--
-- -- | Respects Wikimedia title equality rules: first character is
-- -- case-insensitive, remaining title case-sensitive.
-- instance Eq PageName where
--     PageName a == PageName b =
--         T.toCaseFold (T.take 1 a) == T.toCaseFold (T.take 1 b)
--         && T.drop 1 a == T.drop 1 b
--
-- instance Ord PageName where
--     PageName a `compare` PageName b =
--         case T.toCaseFold (T.take 1 a) `compare` T.toCaseFold (T.take 1 b) of
--           EQ -> T.drop 1 a `compare` T.drop 1 b
--           x -> x
--
-- instance Hashable PageName where
--     hashWithSalt salt (PageName t)
--       | T.null t  = salt
--       | otherwise = hashWithSalt (hashWithSalt salt (T.drop 1 t))
--                                  (toLower $ T.head t)
--
--
-- deriving instance CBOR.Serialise PageName
-- deriving instance FromJSON PageName
-- instance NFData PageName
-- instance FromJSONKey PageName where
--     fromJSONKey = fmap PageName fromJSONKey
-- deriving instance ToJSON PageName
-- instance ToJSONKey PageName where
--     toJSONKey = contramapToJSONKeyFunction (\(PageName n) -> n) toJSONKey
--

