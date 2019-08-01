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

import qualified Data.Text as T

import JSDOM.URLSearchParams
import JavaScript.Web.Location

import CAR.Types
import Types

import qualified Debug.Trace as Debug

mss :: Show a => a -> MisoString
mss = ms . show


data ListModel =
    ListModel { filenames :: [MisoString]
              }
    | ErrorMessageModel { errorMessage :: MisoString
                        }
  deriving (Eq, Show)


emptyModel = ListModel {filenames = [] }


-- | Sum type for application events
data Action
  = SetListing FileListing
  | ReportError MisoString
  | Initialize
  deriving (Show)


-- | Type synonym for an application model
type Model = ListModel


-- | Entry point for a miso application
main :: IO ()
main = startApp App {..}
  where
    initialAction = Initialize -- FetchAssessmentPage "water-distribution" -- initial action to be executed on application load
    model  = emptyModel -- initial model
    update = updateModel          -- update function
    view   = viewModel            -- view function
    events = defaultEvents        -- default delegated events
    subs   = []                   -- empty subscription list
    mountPoint = Nothing          -- mount point for application (Nothing defaults to 'body')




-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model

updateModel (Initialize) m = m <# do
    lst <- fetchJson @FileListing getFileListingPath
--     now' <- getCurrentTime
    return $ case lst of
      Right byteStr -> SetListing byteStr
      Left e  -> ReportError $ mss e

updateModel (SetListing FileListing{filenames = files, pathname = path}) m = noEff $ ListModel {filenames = fmap ms files}

updateModel (ReportError e) m = noEff $ ErrorMessageModel e

-- updateModel x m = m <# do
--     return $ ReportError $ ms ("Unhandled case for updateModel "<> show x <> " " <> show m)



getFileListingPath :: MisoString
getFileListingPath  =
    "/list-l"



data FileListing = FileListing {
        filenames :: [T.Text]
       , pathname :: T.Text
    }
  deriving (Eq, FromJSON, ToJSON, Generic, Show)


data FetchJSONError = XHRFailed XHRError
                    | InvalidJSON String
                    | BadResponse { badRespStatus :: Int, badRespContents :: Maybe BS.ByteString }
                    deriving (Eq, Show)

--fetchJsonL :: forall a. FromJSON a => JSString -> IO (Either FetchJSONError a)  -- todo Either FetchJSONError [a]
--fetchJsonL url = do
--    result <- fetchByteString url
--    case result of
--        Left err          -> pure $ Left err
--        Right byteStr -> pure $ either (Left . InvalidJSON) Right $ eitherDecodeStrict byteStr    -- todo: decode multiple lines
--

fetchJson :: forall a. FromJSON a => JSString -> IO (Either FetchJSONError a)
fetchJson url = do
    result <- fetchByteString url
    case result of
        Left err          -> pure $ Left err
        Right byteStr -> pure $ either (Left . InvalidJSON) Right $ eitherDecodeStrict byteStr


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

squid:: QueryId
squid = QueryId "tqa2:L_0092"

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel m@ListModel{..} =
    div_ []
       [ h1_ [] [text $ "Topic List"]
       , ul_ [] $ fmap renderFile filenames
       ]
  where renderFile :: MisoString -> View Action
        renderFile f =
            let fname = T.pack $ System.FilePath.dropExtension $ fromMisoString f
            in li_ [] [
                  p_ [] [
                    a_ [href_ $ toAssessUrl fname (unQueryId squid) ] [text $ ms fname]
                  ]
              ]
        toAssessUrl f squid =
            ms $ "/assess.html?format=jsonl&q="<>f<>"&squid="<>squid
viewModel ErrorMessageModel { .. }= viewErrorMessage $ ms errorMessage



viewErrorMessage :: MisoString -> View Action
viewErrorMessage msg = div_ []
    [ h1_[] [text $ msg ],
      p_[] [ a_ [ href_ "/"] [text "Back to start..."]]
    ]

