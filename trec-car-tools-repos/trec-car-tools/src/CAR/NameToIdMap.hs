{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CAR.NameToIdMap
    (
    createNameToIdMap
    , openNameToIdMap
    , pageNameToIdMaybeSet
    , pageNameToIdSet
    , pageNameToAnId
    , pageNamesToIdSet
    , openRedirectToIdMap
    , createRedirectToIdMap
    , NameToIdMap
    ) where

import qualified Data.Map as M
import qualified Data.Set as S
import CAR.Types.Files
import CAR.Types
import qualified Codec.CBOR.Read as CBOR.Read
import qualified Codec.Serialise as CBOR
import qualified Data.ByteString.Lazy as BSL
import System.FilePath
import Data.Maybe

newtype NameToIdMap = NameToIdMap (M.Map PageName (S.Set PageId))
                    deriving (CBOR.Serialise)


buildInfoToIdMap :: (Page -> [PageName]) -> FilePath -> IO (NameToIdMap)
buildInfoToIdMap pageToInfo cborPath = do
    (_, pages) <- readPagesOrOutlinesAsPagesWithProvenance cborPath
    return
        $ NameToIdMap
        $ M.fromListWith (<>)
        $ [ (name, S.singleton (pageId page))
          | page <- pages
          , name <- pageToInfo page
          ]



createInfoToIdMap :: (Page -> [PageName]) -> String -> FilePath -> IO ()
createInfoToIdMap transform extension cborPath = do
    index <- buildInfoToIdMap transform cborPath
    BSL.writeFile indexPath $ CBOR.serialise index
  where indexPath = cborPath <.> extension


openInfoToIdMap :: String -> FilePath -> IO NameToIdMap
openInfoToIdMap extension cborPath = do
    index <- either onError snd . CBOR.Read.deserialiseFromBytes CBOR.decode
           <$> BSL.readFile indexPath
    return index
  where
    indexPath = cborPath <.> extension
    onError err =
        error $ "Deserialisation error while deserialising TOC "++show indexPath++": "++show err


openNameToIdMap = openInfoToIdMap "name"
openRedirectToIdMap = openInfoToIdMap "redirect"


createNameToIdMap :: FilePath -> IO ()
createNameToIdMap = createInfoToIdMap  (\p -> [pageName p]) "name"

createRedirectToIdMap :: FilePath -> IO ()
createRedirectToIdMap = createInfoToIdMap  page2redirect  "redirect"
  where
    page2redirect :: Page -> [PageName]
    page2redirect page = (pageName page) : (fromMaybe [] $ getMetadata _RedirectNames (pageMetadata page))



pageNameToIdMaybeSet :: NameToIdMap -> PageName -> Maybe (S.Set PageId)
pageNameToIdMaybeSet (NameToIdMap m) name =
    name `M.lookup` m

pageNameToIdSet :: NameToIdMap -> PageName -> (S.Set PageId)
pageNameToIdSet (NameToIdMap m) name =
    fromMaybe S.empty $ name `M.lookup` m

pageNameToAnId :: NameToIdMap -> PageName -> Maybe PageId
pageNameToAnId (NameToIdMap m) name =
    case name `M.lookup` m of
        Nothing -> Nothing
        Just xs | S.null xs -> Nothing
        Just xs -> Just (head $ S.toList xs)


pageNamesToIdSet :: NameToIdMap -> [PageName] -> S.Set PageId
pageNamesToIdSet m = foldMap (pageNameToIdSet m)