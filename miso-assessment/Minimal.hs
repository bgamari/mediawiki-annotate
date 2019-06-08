module Main where

import Miso
import Miso.String

import qualified Data.Text as T

data StringModel =
    StringModel
  deriving (Eq)

emptyModel = StringModel

data Action
  = Initialize

type Model = StringModel

main :: IO ()
main = startApp App {
    initialAction = Initialize
    , model  = emptyModel
    , update = updateModel
    , view   = viewModel
    , events = defaultEvents
    , subs   = []
    , mountPoint = Just $ ms "body"
  }

updateModel :: Action -> Model -> Effect Action Model


updateModel (Initialize) m = noEff $ StringModel

viewModel :: Model -> View Action
viewModel StringModel =
    div_ [] [p_ [] [text $ ms "hello"]]

