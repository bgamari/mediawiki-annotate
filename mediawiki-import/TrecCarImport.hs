{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Data.Monoid
import System.IO

import qualified Data.Yaml as Yaml
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Codec.Serialise as CBOR
import qualified Codec.Serialise.Encoding as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.Binary as B
import Pipes
import qualified Pipes.Prelude as PP
import qualified Control.Concurrent.ForkMap as CM
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashSet as HS
import qualified Data.Text.Encoding as T


import Development.GitRev
import Options.Applicative

import Data.MediaWiki.XmlDump (NamespaceId, Format, WikiDoc(..), parseWikiDocs)
import qualified Data.MediaWiki.XmlDump as XmlDump
import CAR.Types
import CAR.Import
import CAR.Import.ConfigFile
import CAR.ToolVersion
import Data.MediaWiki.XmlDump

newtype EncodedCbor a = EncodedCbor {getEncodedCbor :: BSL.ByteString}

instance (CBOR.Serialise a) => B.Binary (EncodedCbor a) where
    get = EncodedCbor <$> B.get
    put = B.put . getEncodedCbor

encodedCbor :: CBOR.Serialise a => a -> EncodedCbor a
encodedCbor = EncodedCbor . CBOR.serialise

instance B.Binary NamespaceId
instance B.Binary Format
instance B.Binary XmlDump.PageId
instance B.Binary WikiDoc

commit :: T.Text
commit = $(gitHash)

opts :: Parser (Int, Maybe FilePath, SiteId -> Provenance)
opts =
    (,,)
       <$> option auto (short 'j' <> long "jobs" <> metavar "N" <> help "Number of workers" <> value 1)
       <*> optional (option str (short 'c' <> long "config" <> metavar "CONFIG" <> help "Configuration file"))
       <*> prov
  where
    text = T.pack <$> str
    prov = do
        sourceName <- option str (short 'D' <> long "dump-date" <> metavar "DATE" <> help "Wikipedia dump date" <> value "unknown")
        dataReleaseName <- option str (short 'N' <> long "release-name" <> metavar "NAME" <> help "Data release name" <> value "unknown")
        comments <- many $ option str (short 'C' <> long "comments" <> metavar "NAME" <> help "Other comments about data release")
        language <- option (Language <$> text) (short 'L' <> long "language" <> metavar "LANG" <> help "The IETF language code of the text collection" <> value "en-US")
        pure $ \provSiteId ->
            Provenance { siteProvenances = [ SiteProvenance {siteComments = [], ..} ]
                       , dataReleaseName = dataReleaseName
                       , comments = comments
                       , transforms = [transform "trec-car-import" commit ()] -- TODO
                       }


isInteresting :: SiteInfo -> WikiDoc -> Bool
isInteresting siteInfo  =
-- lookup (XmlDump.NamespaceId 14) (XmlDump.siteNamespaces siteInfo)
    let interestingNamesSpaceId = [NamespaceId 0, NamespaceId 14] -- (0, Article), (14, Category), (100, Portal)
        uninterestingNameSpaces = HS.fromList
                                  $ [ T.encodeUtf8 nameSpaceText
                                  | (nsId, name) <- (XmlDump.siteNamespaces siteInfo)
                                  , not $ (nsId `elem` interestingNamesSpaceId)
                                  , Namespace nameSpaceText <- pure name
                                  ]
    in \WikiDoc{..} ->
        case (==':') `BS.break` docTitle of
              (prefix, rest) | not $ BS.null rest -> not $ prefix `HS.member` uninterestingNameSpaces
              _                                -> True

_isEnInteresting :: WikiDoc -> Bool
_isEnInteresting WikiDoc{..} = not $
       "Category talk:" `BS.isPrefixOf` docTitle
    || "Talk:" `BS.isPrefixOf` docTitle
    || "File:" `BS.isPrefixOf` docTitle
    || "File talk:" `BS.isPrefixOf` docTitle
    || "Special:" `BS.isPrefixOf` docTitle
    || "User:" `BS.isPrefixOf` docTitle
    || "User talk:" `BS.isPrefixOf` docTitle
    || "Wikipedia talk:" `BS.isPrefixOf` docTitle
    || "Wikipedia:" `BS.isPrefixOf` docTitle
    || "Template:" `BS.isPrefixOf` docTitle
    || "Template talk:" `BS.isPrefixOf` docTitle
    || "Portal:" `BS.isPrefixOf` docTitle
    || "Module:" `BS.isPrefixOf` docTitle
    || "Draft:" `BS.isPrefixOf` docTitle
    || "Help:" `BS.isPrefixOf` docTitle
    || "Book:" `BS.isPrefixOf` docTitle
    || "TimedText:" `BS.isPrefixOf` docTitle
    || "MediaWiki:" `BS.isPrefixOf` docTitle


main :: IO ()
main = do
    (workers, maybeConfig, prov) <- execParser' 1 (helper <*> opts) mempty
    (siteInfo, docs) <- parseWikiDocs <$> BSL.getContents

    let Just (XmlDump.Namespace categoryNamespaceName) =
            lookup (XmlDump.NamespaceId 14) (XmlDump.siteNamespaces siteInfo)
    config <- case maybeConfig of
      Just configPath -> do
          configFile <- Yaml.decodeFileEither configPath >>= either (fail . show) return
          return $ configFileToConfig categoryNamespaceName configFile
      Nothing         -> return defaultConfig

    let siteId = SiteId $ XmlDump.siteDbName siteInfo
        isInterestingPage = (isInteresting siteInfo)

        parsed :: Producer (Either String (EncodedCbor Page)) IO ()
        parsed =
            CM.map (2*workers) workers
                (fmap encodedCbor . toPage config siteId)
                (each $ filter isInterestingPage docs)
        putParsed (Left err) = hPutStrLn stderr $ "\n"<>err
        putParsed (Right page) = BSL.putStr (getEncodedCbor page) >> hPutStr stderr "."

    BSL.putStr $ CBOR.toLazyByteString
        $ CBOR.encode (Header { headerType = PagesFile
                              , provenance = prov siteId
                              })
       <> CBOR.encodeListLenIndef
    runEffect $ parsed >-> PP.mapM_ putParsed
    BSL.putStr $ CBOR.toLazyByteString $ CBOR.encodeBreak
