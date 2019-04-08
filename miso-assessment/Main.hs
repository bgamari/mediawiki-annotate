-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import Miso
import Miso.String
import Data.Aeson
import JavaScript.Web.XMLHttpRequest
import Control.Exception
import qualified Data.ByteString as BS
import Data.Maybe

import JSDOM.URLSearchParams
import JavaScript.Web.Location

import CAR.Types
import Types

import qualified Debug.Trace as Debug


data AssessmentModel =
    AssessmentModel { page :: AssessmentPage }
    | FileNotFoundErrorModel { filename :: MisoString }
    | ErrorMessageModel { errorMessage :: MisoString }
    | LoadingPageModel
  deriving (Eq)


-- | Sum type for application events
data Action
  = FetchAssessmentPage String
  | SetAssessmentPage AssessmentPage
  | ReportError MisoString
  | Initialize
  deriving (Show)




emptyAssessmentPage :: AssessmentPage
emptyAssessmentPage = AssessmentPage {
        apTitle = "",
        apRunId = "",
        apSquid = QueryId "",
        apQueryFacets = [],
        apParagraphs = []
    }

emptyAssessmentModel :: AssessmentModel
emptyAssessmentModel = LoadingPageModel





-- | Type synonym for an application model
type Model = AssessmentModel


-- | Entry point for a miso application
main :: IO ()
main = startApp App {..}
  where
    initialAction = Initialize -- FetchAssessmentPage "water-distribution" -- initial action to be executed on application load
    model  = LoadingPageModel  -- initial model
    update = updateModel          -- update function
    view   = viewModel            -- view function
    events = defaultEvents        -- default delegated events
    subs   = []                   -- empty subscription list
    mountPoint = Nothing          -- mount point for application (Nothing defaults to 'body')

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel (FetchAssessmentPage pageName) m = m <# do
    page <- fetchJson $ getAssessmentPageFilePath pageName

    return $ case page of 
      Right p -> SetAssessmentPage p
      Left e  -> ReportError $ ms $ show e
updateModel (Initialize) m = m <# do
    loc <- getWindowLocation >>= getSearch
    params <- newURLSearchParams loc
    maybeQ <- get params ("q" :: MisoString)
    let query = fromMaybe "default" maybeQ
    return $ FetchAssessmentPage query

updateModel (SetAssessmentPage p) _ = noEff $ AssessmentModel p
updateModel (ReportError e) _ = noEff $ ErrorMessageModel e

-- updateModel Home m = m <# do
--                h <- windowInnerHeight
--                return $ UpdateModelWithWindow h
-- updateModel (UpdateModelWithWindow h) m = noEff (h)
-- updateModel SayHelloWorld m = m <# do
--   putStrLn "Hello World" >> pure NoOp




getAssessmentPageFilePath :: String -> MisoString
getAssessmentPageFilePath pageName =
    ms
--     $ "http://trec-car.cs.unh.edu:8080/data/" <> pageName <> ".json"
    $ "http://localhost:8000/data/" <> pageName <> ".json"

data FetchJSONError = XHRFailed XHRError
                    | InvalidJSON String
                    | BadResponse { badRespStatus :: Int, badRespContents :: Maybe BS.ByteString }
                    deriving (Eq, Show)

fetchJson :: forall a. FromJSON a => JSString -> IO (Either FetchJSONError a)
fetchJson url = do
    resp <- handle onError $ fmap Right $ xhrByteString req
    case resp of
      Right (Response{..})
        | status == 200
        , Just d <- contents -> pure $ either (Left . InvalidJSON) Right $ eitherDecodeStrict d
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

-- ------------- Presentation ----------------------

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel AssessmentModel{ page= AssessmentPage{..}} = div_ []
--     H.head prologue
   [ h1_ [] [text $ ms apTitle]
   , p_ [] [text "Query Id: ", text $ ms $ unQueryId apSquid]
   , p_ [] [text "Run: ", text $ ms apRunId]
   , hr_ []
   , ol_ [] $ fmap (renderParagraph) apParagraphs
   ]
  where wrapLi v = li_ [] [v]
        renderParagraph :: Paragraph -> View action
        renderParagraph Paragraph{..} =
            li_ [class_ "entity-snippet-li"] [
                p_ [] [
                    span_ [class_ "annotation"][ -- data-item  data-query
                        div_ [class_ "btn-toolbar annotate" ] [ -- data_ann_od role_ "toolbar"
                            div_ [class_ "btn-group"] [         --  , role_ "toolbar"
                                button_ [class_ "btn btn-sm" ][text "Must"]   -- data-value="5"
                                , button_ [class_ "btn btn-sm" ][text "Should"]   -- data-value="4"
                            ]
                        ]
                    ]
                    , p_ [class_ "paragraph-id"] [text $ ms $ unpackParagraphId paraId]
                    , p_ [class_ "entity-snippet-li-text"] $ fmap renderParaBody paraBody
                ]
            ]
        renderParaBody :: ParaBody -> View action
        renderParaBody (ParaText txt) = text $ ms txt
        renderParaBody (ParaLink Link{..}) = a_ [ href_ $ ms $ unpackPageName linkTarget] [text $ ms linkAnchor]

viewModel LoadingPageModel = viewErrorMessage $ "Loading Page"
viewModel FileNotFoundErrorModel { .. }= viewErrorMessage $ "File not Found " <> ms filename
viewModel ErrorMessageModel { .. }= viewErrorMessage $ ms errorMessage



viewErrorMessage :: MisoString -> View Action
viewErrorMessage msg = div_ []
    [ h1_[] [text $ msg ],
      p_[] [ a_ [ href_ "/"] [text "Back to start..."]]
    ]

