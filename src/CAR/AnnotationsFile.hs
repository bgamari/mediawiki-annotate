module CAR.AnnotationsFile where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import System.FilePath
import qualified Data.HashMap.Strict as HM
import System.IO.MMap
import qualified Data.Aeson as JSON
import qualified Data.Binary.Serialise.CBOR as CBOR
import CAR.Types
import Data.MediaWiki.Markup

type Offset = Int
data AnnotationsFile = AnnotationsFile BS.ByteString (HM.HashMap PageName Offset)

openAnnotations :: FilePath -> IO AnnotationsFile
openAnnotations fname = do
    cbor <- mmapFileByteString fname Nothing
    mtoc <- JSON.decode <$> LBS.readFile (fname <.> "json")
    case mtoc of
      Just toc -> return $ AnnotationsFile cbor toc
      Nothing  -> fail $ "openAnnotations: Failed to open TOC for "++fname


lookupPage :: PageName -> AnnotationsFile -> Maybe Page
lookupPage name (AnnotationsFile cbor toc) =
    deserialise <$> HM.lookup name toc
  where
    deserialise offset = CBOR.deserialise $ LBS.fromStrict $ BS.drop offset cbor

pageNames :: AnnotationsFile -> [PageName]
pageNames (AnnotationsFile _ h) = HM.keys h

pages :: AnnotationsFile -> [Page]
pages (AnnotationsFile b _) =
    decodeCborList (LBS.fromStrict b)
