{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import qualified Codec.Serialise as CBOR
import qualified Data.JsonStream.Parser as JS
import CAR.Types (PageName(..), SiteId(..))
import WikiData

buildIndex :: BSL.ByteString -> LangIndex
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
    BSL.getContents >>= pure . buildIndex >>= BSL.writeFile "language-index.cbor" . CBOR.serialise
