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

import qualified Data.Text as T

import JSDOM.URLSearchParams
import JavaScript.Web.Location


mss :: Show a => a -> MisoString
mss = ms . show


data StringModel =
    StringModel {text :: T.Text}
  deriving (Eq, Show)


emptyModel = StringModel {displayText = "Initializing..."}


-- | Sum type for application events
data Action
  = Initialize
  deriving (Show)


-- | Type synonym for an application model
type Model = StringModel


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


updateModel (Initialize) m = noEff $ StringModel "OK, Computer!"



-- ------------- Presentation ----------------------

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel m@StringModel{..} =
    div_ []
       [ h1_ [] [text $ displayText ]
       ]



