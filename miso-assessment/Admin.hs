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
import qualified System.FilePath


import qualified Data.Text as T

import JSDOM.URLSearchParams
import JavaScript.Web.Location


import Types

import qualified Debug.Trace as Debug

mss :: Show a => a -> MisoString
mss = ms . show



getUsernameUrl :: MisoString
getUsernameUrl = "/username"



data AdminModel =
    AdminModel {username :: Maybe UserId}
    | ErrorMessageModel { errorMessage :: MisoString
                        }
  deriving (Eq, Show)


emptyModel = AdminModel { username = Nothing}


-- | Sum type for application events
data Action
  = Initialize
  | InitUserName (Maybe UserId)
  | Noop
  | ClearAllAssessments
  deriving (Show)


-- | Type synonym for an application model
type Model = AdminModel


-- | Entry point for a miso application
main :: IO ()
main = startApp App {..}
  where
    initialAction =  Initialize
    model  = emptyModel -- initial model
    update = updateModel          -- update function
    view   = viewModel            -- view function
    events = defaultEvents        -- default delegated events
    subs   = []                   -- empty subscription list
    mountPoint = Nothing          -- mount point for application (Nothing defaults to 'body')


data FetchJSONError = XHRFailed XHRError
                    | InvalidJSON String
                    | BadResponse { badRespStatus :: Int, badRespContents :: Maybe BS.ByteString }
                    deriving (Eq, Show)


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

getUserName :: IO (Maybe UserId)
getUserName = do
    username' <- fetchByteString $ getUsernameUrl
    return $ case username' of
      Right username -> Just $ decodeByteString username
      Left _e -> Nothing



-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model

updateModel (Initialize) m = m <# do
    username <- getUserName
    return $ InitUserName username

updateModel (InitUserName username) m@AdminModel{} = noEff
    m {username=username}

updateModel (InitUserName username) m = noEff m

updateModel Noop m = noEff m

updateModel ClearAllAssessments m = m <# do
    clearLocalStorage
    return Noop



-- ------------- Presentation ----------------------



-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel m@AdminModel{..} =
    div_ []
           [ p_ [] [a_[href_ "/index.html"][text $ "To Start Page..."]]
           , h1_ [] [text $ "Admin Functions"]
           , button_ [class_ "btn-sm", onClick ClearAllAssessments] [text "Clear Browser Cache"]
           , p_ [] [text$ renderUserName username]
           , p_ [] [a_ [href_ "/logout.html"][text $ "Logout"]]
           ]
  where renderUserName :: Maybe UserId -> MisoString
        renderUserName (Just username) = ms $ ("Logged in as " <> username)
        renderUserName (Nothing) = "Not logged in"


viewModel ErrorMessageModel { .. }= viewErrorMessage $ ms errorMessage



viewErrorMessage :: MisoString -> View Action
viewErrorMessage msg = div_ []
    [ h1_[] [text $ msg ],
      p_[] [ a_ [ href_ "/"] [text "Back to start..."]]
    ]

