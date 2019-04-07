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



statusAssessmentPage :: MisoString -> AssessmentPage
statusAssessmentPage status = AssessmentPage {
        apTitle = fromMisoString status,
        apRunId = "",
        apSquid = QueryId "",
        apQueryFacets = [],
        apParagraphs = []
    }

-- | Type synonym for an application model
type Model = AssessmentPage

-- | Sum type for application events
data Action
  = FetchAssessmentPage String
  | SetAssessmentPage AssessmentPage
  | ReportError MisoString
  | Initialize
  deriving (Show)

-- | Entry point for a miso application
main :: IO ()
main = startApp App {..}
  where
    initialAction = Initialize -- FetchAssessmentPage "water-distribution" -- initial action to be executed on application load
    model  = statusAssessmentPage "Loading page..."  -- initial model
    update = updateModel          -- update function
    view   = viewModel            -- view function
    events = defaultEvents        -- default delegated events
    subs   = []                   -- empty subscription list
    mountPoint = Nothing          -- mount point for application (Nothing defaults to 'body')

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel (FetchAssessmentPage pageName) m = m <# do
    page <- getAssessmentPage pageName
    return $ case page of 
      Right p -> SetAssessmentPage p
      Left e  -> ReportError $ ms e
updateModel (Initialize) m = m <# do
    loc <- getWindowLocation >>= getHref
    params <- newURLSearchParams loc
    maybeQ <- get params ("q" :: MisoString)
    let query = fromMaybe "default" maybeQ
    return $ FetchAssessmentPage query

updateModel (SetAssessmentPage p) _ = noEff p
updateModel (ReportError e) _ = noEff $ statusAssessmentPage e

-- updateModel Home m = m <# do
--                h <- windowInnerHeight
--                return $ UpdateModelWithWindow h
-- updateModel (UpdateModelWithWindow h) m = noEff (h)
-- updateModel SayHelloWorld m = m <# do
--   putStrLn "Hello World" >> pure NoOp

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel AssessmentPage{..} = div_ []
--     H.head prologue
   [ h1_ [] [text $ ms apTitle]
   , p_ [] [text "Query Id: ", text $ ms $ unQueryId apSquid]
   , p_ [] [text "Run: ", text $ ms apRunId]
   , hr_ []
   , ol_ [] $ fmap (wrapLi . renderParagraph) apParagraphs
   ]
  where wrapLi v = li_ [] [v]
        renderParagraph :: Paragraph -> View action
        renderParagraph Paragraph{..} = div_ []
          [ p_ [] [text $ ms $ unpackParagraphId paraId]
          , p_ [] $ fmap renderParaBody paraBody
          ]
        renderParaBody :: ParaBody -> View action
        renderParaBody (ParaText txt) = text $ ms txt
        renderParaBody (ParaLink Link{..}) = a_ [ href_ $ ms $ unpackPageName linkTarget] [text $ ms linkAnchor]


--
-- -- === Pretty section Path ====
-- viewHeader :: SectionPathWithName -> H.Html
-- viewHeader spr@SectionPathWithName{..} = do
--     div_ [class_ "overview-heading-query"] [
--         text $ prettySectionPath spr,
--
--         p_ [] [ text $ wikipediaSection spr "Wikipedia" ],
--
--         div_ [] [
--             span_ "Query ID: " ,
--             code_ $ text_ (escapeSectionPath sprQueryId)
--         ],
--
--         div_ [
--             p_  [] [
--                 text "Back to ",
--                 a_ [href_ "./"] $ text "Outline",
--                 text " / ",
--                 a_  [HA.href "../"] $ text "Topic List"
--             ]
--         ]
--     ]


getAssessmentPage :: String -> IO (Either String AssessmentPage)
getAssessmentPage pageName =
    fetchJson $ ms
--     $ "http://trec-car.cs.unh.edu:8080/data/" <> pageName <> ".json"
    $ "http://localhost:8000/data/" <> pageName <> ".json"

fetchJson :: forall a. FromJSON a => JSString -> IO (Either String a)
fetchJson url = do
    resp <- handle onError $ fmap Right $ xhrByteString req
    case resp of
      Right (Response{..})
        | status == 200
        , Just d <- contents -> pure $ eitherDecodeStrict d
        | otherwise          -> pure $ Left "failed response"
      Left err               -> pure $ Left err
  where
    onError :: XHRError -> IO (Either String (Response BS.ByteString))
    onError = pure . Left . show
    req = Request { reqMethod = GET
                  , reqURI = url
                  , reqLogin = Nothing
                  , reqHeaders = []
                  , reqWithCredentials = False
                  , reqData = NoData
                  }

