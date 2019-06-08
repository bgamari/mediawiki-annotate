module Main where

import Miso
import Miso.String hiding (concatMap, filter, length, zip)

import qualified Data.Text as T

data StringModel =
    StringModel {displayText :: T.Text}
  deriving (Eq)

emptyModel = StringModel {displayText = T.pack "Initializing..."}

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


updateModel (Initialize) m = noEff $ StringModel {displayText = T.pack "OK, Computer!"}

viewModel :: Model -> View Action
viewModel m@StringModel{displayText=displayText} =
    div_ []
       [ h1_ [] [text $ ms $ displayText ]
       ]



