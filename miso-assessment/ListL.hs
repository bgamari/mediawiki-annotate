-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric#-}
{-# LANGUAGE DuplicateRecordFields#-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import Miso
import Miso.String hiding (concatMap, filter, length, zip)
import Data.Aeson
import GHC.Generics
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import JavaScript.Web.XMLHttpRequest

import Control.Exception
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy
-- import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.List as L
import Data.Maybe
import Data.Time
import Data.Char
import Data.String
import Language.Porter
import qualified System.FilePath

import qualified Codec.Serialise as CBOR
import qualified CAR.Types.CborList as CBOR
import qualified CAR.Types.Files as CAR

import qualified Data.Text as T

import JSDOM.URLSearchParams
import JavaScript.Web.Location


import CAR.Types
import Types

import qualified Debug.Trace as Debug

mss :: Show a => a -> MisoString
mss = ms . show



getFileListingPath :: MisoString
getFileListingPath  =
    "/list-l"

getAssignmentsPath :: MisoString
getAssignmentsPath  =
    "/data/assessor_assignments.json"


cborFileName :: MisoString
cborFileName =
    "benchmarkY3test.cbor"

getUsernameUrl :: MisoString
getUsernameUrl = "/username"



instance Eq Page where
    p1 == p2 = (pageId p1) == (pageId p2)

data ListModel =
    ListModel { filenames :: [MisoString]
              , pages :: [Page]
              , assessorSquids :: [QueryId]
              , username :: Maybe UserId
              }
    | ErrorMessageModel { errorMessage :: MisoString
                        }
  deriving (Eq, Show)


emptyModel = ListModel {filenames = [], pages = [], assessorSquids = [], username = Nothing }

-- | Sum type for application events
data Action
  = SetListing FileListing [QueryId] (Maybe UserId)
  | ReportError MisoString
  | Initialize
  | LoadCbor JSString
  | SetTqaPages [Page]
  deriving (Show)


-- | Type synonym for an application model
type Model = ListModel


-- | Entry point for a miso application
main :: IO ()
main = startApp App {..}
  where
    initialAction =  LoadCbor ("./data/" <> cborFileName)
    model  = emptyModel -- initial model
    update = updateModel          -- update function
    view   = viewModel            -- view function
    events = defaultEvents        -- default delegated events
    subs   = []                   -- empty subscription list
    mountPoint = Nothing          -- mount point for application (Nothing defaults to 'body')


getUserName :: IO (Maybe UserId)
getUserName = do
    username' <- fetchByteString $ getUsernameUrl
    return $ case username' of
      Right username -> Just $ decodeByteString username
      Left _e -> Nothing



-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model

updateModel (LoadCbor filename) m = m <# do
    cbor <- fetchCbor filename
            :: IO (Either FetchJSONError [Page])
    return $ case cbor of
      Right pages -> SetTqaPages pages
      Left e  -> ReportError $ mss e

updateModel (SetTqaPages pages) _ =  ListModel { pages = pages, filenames = mempty, assessorSquids = mempty, username = Nothing}  <# return Initialize

updateModel (Initialize) m = m <# do
    loc <- getWindowLocation >>= getSearch
    params <- newURLSearchParams loc
    maybeUserName <- get params ("username" :: MisoString)

    authenticatedUsername <- getUserName
    let username = Debug.traceShowId $ case maybeUserName of
                        Just _ -> maybeUserName
                        Nothing -> authenticatedUsername
    assessorData' <- fetchJson @AssessorAssignmentData getAssignmentsPath
    case assessorData' of
        Left e -> return $ ReportError $ mss e
        Right assessorData -> do
            let assessorSquids = L.nub
                               $ L.concat [ squids
                                          | AssessorAssignments{ user_id = assessor, squids = squids } <- assignments assessorData
                                          , let Just username' = username
                                          , assessor == username'
                                          ]
            lst <- fetchJson @FileListing getFileListingPath
        --     now' <- getCurrentTime
            return $ Debug.traceShow ("assessorSquids "<> show assessorSquids) $ case lst of
              Right byteStr -> SetListing byteStr assessorSquids username
              Left e  -> ReportError $ mss e

updateModel (SetListing FileListing{filenames = files, pathname = path} assessorSquids username) m@ListModel{} =
    noEff $ (m {filenames = fmap ms files, assessorSquids = assessorSquids, username = username} :: ListModel)
updateModel (SetListing _ _ _ ) m = noEff $ m

updateModel (ReportError e) m = noEff $ ErrorMessageModel e

-- updateModel x m = m <# do
--     return $ ReportError $ ms ("Unhandled case for updateModel "<> show x <> " " <> show m)


data AssessorAssignmentData = AssessorAssignmentData {
        assignments :: [AssessorAssignments]
    }
  deriving (Eq, FromJSON, ToJSON, Generic, Show)

data AssessorAssignments = AssessorAssignments {
        user_id :: UserId,
        squids :: [QueryId]
    }
  deriving (Eq, FromJSON, ToJSON, Generic, Show)

data FileListing = FileListing {
        filenames :: [T.Text]
       , pathname :: T.Text
    }
  deriving (Eq, FromJSON, ToJSON, Generic, Show)


data FetchJSONError = XHRFailed XHRError
                    | InvalidJSON String
                    | BadResponse { badRespStatus :: Int, badRespContents :: Maybe BS.ByteString }
                    deriving (Eq, Show)


fetchJson :: forall a. FromJSON a => JSString -> IO (Either FetchJSONError a)
fetchJson url = do
    result <- fetchByteString url
    case result of
        Left err          -> pure $ Left err
        Right byteStr -> pure $ either (Left . InvalidJSON) Right $ eitherDecodeStrict byteStr


fetchCbor :: forall a. CBOR.Serialise a => JSString -> IO (Either FetchJSONError [a])
fetchCbor url = do
    result <- fetchByteString url
    case result of
        Left err        -> pure $ Left err
        Right byteStr   -> let hdr ::  CAR.Header
                               (hdr, pages) = CBOR.decodeCborList $ Data.ByteString.Lazy.fromStrict byteStr
                           in pure $ Right pages



fetchByteString:: JSString -> IO (Either FetchJSONError BS.ByteString)
fetchByteString url = do
    resp <- handle onError $ fmap Right $ xhrByteString req
    case resp of
      Right (Response{..})
        | status == 200
        , Just c <- contents -> pure $ Right c
      Right resp             -> pure $ Left $ BadResponse (status resp) (contents resp)
      Left err               -> pure $ Left $ XHRFailed err
  where
    onError :: XHRError -> IO (Either XHRError (Response BS.ByteString))
    onError = pure . Left

    req = Request { reqMethod = GET
                  , reqURI = url
                  , reqLogin = Nothing
                  , reqHeaders = []
                  , reqWithCredentials = False
                  , reqData = NoData
                  }

decodeByteString :: BS.ByteString -> T.Text
decodeByteString = Data.Text.Encoding.decodeUtf8

-- ------------- Presentation ----------------------


-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel m@ListModel{..} =
    let selectedPages =
            case assessorSquids of
                []     -> pages
                squids -> let pageMap =  M.fromList [(QueryId $ T.pack $ unpackPageId $ pageId page , page) | page <- pages]
                          in catMaybes $ [ squid `M.lookup` pageMap | squid <- squids ]

    in div_ []
           [ h1_ [] [text $ (runListTitle username)]
           , ul_ [] $ fmap renderTopics  selectedPages
           ]
  where renderTopics page =
            li_ [] [ h2_ [] [text $ ms $ unpackPageName $ pageName page]
                   , p_ [] [text $ ms $ unpackPageId $ pageId page]
                   , ul_ [] $ (renderGold (pageId page)  : fmap (renderFile (pageId page) ) filenames)
                   ]

        renderFile :: PageId -> MisoString -> View Action
        renderFile pageId f =
            let fname = T.pack $ System.FilePath.dropExtension $ fromMisoString f
            in li_ [] [
                  p_ [] [
                    a_ [href_ $ toAssessUrl fname (T.pack $ unpackPageId pageId) ] [text $ ms fname]
                  ]
              ]
        renderGold :: PageId -> View Action
        renderGold pageId =
            li_ [] [
                  p_ [] [
                    a_ [href_ $ toGoldUrl (ms $ unpackPageId pageId) ] [text $ ("gold article"::MisoString)]
                  ]
              ]
        toAssessUrl f squid =
            ms $ "/assess.html?format=jsonl&q="<>f<>"&squid="<>squid
        toGoldUrl squid =
            ms $ "/gold.html?&cbor="<>cborFileName<>"&squid="<>squid

        runListTitle :: Maybe UserId -> MisoString
        runListTitle Nothing = "Topics and Runs for Assessment"
        runListTitle (Just name) = "Topics and Runs for Assessment for "<> (ms $ name)
viewModel ErrorMessageModel { .. }= viewErrorMessage $ ms errorMessage



viewErrorMessage :: MisoString -> View Action
viewErrorMessage msg = div_ []
    [ h1_[] [text $ msg ],
      p_[] [ a_ [ href_ "/"] [text "Back to start..."]]
    ]

