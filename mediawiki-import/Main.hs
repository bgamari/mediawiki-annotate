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
import qualified Codec.Serialise as CBOR
import qualified Codec.Serialise.Encoding as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.Binary as B
import Pipes
import qualified Pipes.Prelude as PP
import qualified Control.Concurrent.ForkMap as CM

import Development.GitRev
import Options.Applicative

import Data.MediaWiki.XmlDump (NamespaceId, Format, WikiDoc(..), parseWikiDocs)
import qualified Data.MediaWiki.XmlDump as XmlDump
import CAR.Types
import Import
import ConfigFile

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

commit :: String
commit = $(gitHash)

opts :: Parser (Int, Maybe FilePath, SiteId -> Provenance)
opts =
    (,,)
       <$> option auto (short 'j' <> long "jobs" <> metavar "N" <> help "Number of workers" <> value 1)
       <*> optional (option str (short 'c' <> long "config" <> metavar "CONFIG" <> help "Configuration file"))
       <*> prov
  where
    prov = do
        wikiDumpDate <- option str (short 'D' <> long "dump-date" <> metavar "DATE" <> help "Wikipedia dump date" <> value "unknown")
        dataReleaseName <- option str (short 'N' <> long "release-name" <> metavar "NAME" <> help "Data release name" <> value "unknown")
        comments <- fmap unlines $ many $ option str (short 'C' <> long "comments" <> metavar "NAME" <> help "Other comments about data release")
        return (\wikiSite -> Provenance {toolsCommit = commit, ..})

main :: IO ()
main = do
    (workers, maybeConfig, prov) <- execParser $ info (helper <*> opts) mempty
    (siteInfo, docs) <- parseWikiDocs <$> BSL.getContents

    let Just (XmlDump.Namespace categoryNamespaceName) =
            lookup (XmlDump.NamespaceId 14) (XmlDump.siteNamespaces siteInfo)
    config <- case maybeConfig of
      Just configPath -> do
          configFile <- Yaml.decodeFileEither configPath >>= either (fail . show) return
          return $ configFileToConfig categoryNamespaceName configFile
      Nothing         -> return defaultConfig

    let siteId = SiteId $ XmlDump.siteDbName siteInfo

        parsed :: Producer (Either String (EncodedCbor Page)) IO ()
        parsed =
            CM.map (2*workers) workers
                (fmap encodedCbor . toPage config siteId)
                (each $ filter isInteresting docs)
        putParsed (Left err) = hPutStrLn stderr $ "\n"<>err
        putParsed (Right page) = BSL.putStr (getEncodedCbor page) >> hPutStr stderr "."

    BSL.putStr $ CBOR.toLazyByteString
        $ CBOR.encode (Header { headerType = PagesFile
                              , provenance = prov siteId
                              })
       <> CBOR.encodeListLenIndef
    runEffect $ parsed >-> PP.mapM_ putParsed
    BSL.putStr $ CBOR.toLazyByteString $ CBOR.encodeBreak
