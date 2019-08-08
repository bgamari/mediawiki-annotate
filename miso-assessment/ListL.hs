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
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy
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
import PageStats

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

--type RunId = T.Text


instance Eq Page where
    p1 == p2 = (pageId p1) == (pageId p2)

data ListModel =
    ListModel { filenames :: [MisoString]
              , pages :: [Page]
              , assessorSquids :: [QueryId]
              , username :: Maybe UserId
              , missingStatsMap :: M.Map (QueryId,RunId) MissingAssessmentStats
              }
    | ErrorMessageModel { errorMessage :: MisoString
                        }
  deriving (Eq, Show)



emptyModel = ListModel {filenames = [], pages = [], assessorSquids = [], username = Nothing, missingStatsMap = mempty }

-- | Sum type for application events
data Action
  = SetListing FileListing [QueryId] (Maybe UserId)
  | ReportError MisoString
  | Initialize
  | LoadCbor JSString
  | SetCborPages [Page]
  | LoadRuns [MisoString] (M.Map (QueryId, RunId) MissingAssessmentStats)
  | Noop
--  | SyncLocalModel
--  | SetMissingState (M.Map (PageId, RunId) MissingAssessmentStats)
  deriving (Show)


-- | Type synonym for an application model
type Model = ListModel


-- | Entry point for a miso application
main :: IO ()
main = startApp App {..}
  where
    initialAction =  Initialize -- LoadCbor ("./data/" <> cborFileName)
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



pageIdToQueryId :: PageId -> QueryId
pageIdToQueryId pageId =  QueryId $ T.pack $ unpackPageId $ pageId


-- sync with version in Main.hs
storageKeyQuery :: UserId -> QueryId -> MisoString
storageKeyQuery userId queryId =
    (ms $ userId) <> "-" <> (ms $ unQueryId queryId)



loadLocalModel :: UserId -> QueryId -> JSM (Maybe AssessmentState)
loadLocalModel userId queryId = do
    let key = storageKeyQuery userId queryId
    oldStateMaybe <- getLocalStorage key
                        :: JSM (Either String AssessmentState)

    let oldState =
            case oldStateMaybe of
                Right old -> Just old
                Left _msg -> Nothing
    return oldState



-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model



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
    (m {filenames = fmap ms files, assessorSquids = assessorSquids, username = username} :: ListModel) <# do
    return $ LoadCbor ("./data/" <> cborFileName)


updateModel (LoadCbor filename) m@ListModel{..} = m <# do
    cbor <- fetchCbor filename
            :: IO (Either FetchJSONError [Page])

    return $ case cbor of
      Right allPages -> do
                        let selectedPages =
                                  case assessorSquids of
                                      []     -> allPages
                                      squids -> let pageMap =  M.fromList [(QueryId $ T.pack $ unpackPageId $ pageId page , page) | page <- allPages]
                                                in catMaybes $ [ squid `M.lookup` pageMap | squid <- squids ]
                        SetCborPages selectedPages
      Left e  -> ReportError $ mss e

updateModel (SetCborPages pages') m@ListModel{..} =  newModel <# do
    return $ LoadRuns filenames mempty

  where newModel = m { pages = pages'}



updateModel (LoadRuns files prevMissingStats) m@ListModel{..} = newModel <#
    case files of
        []            -> return Noop
        pageName:rest -> do
                apPages <- fetchJsonL $ getAssessmentPageFilePath  $ fromMisoString pageName
                         :: IO (Either FetchJSONError [AssessmentPage])

                case apPages of
                    Left e  -> return $ ReportError $ ms $ show e
                    Right apPages' -> do
                        let queryIds = S.fromList [ pageIdToQueryId $ pageId p | p <- pages]
                            filteredPages =
                                [ ap
                                | ap <- apPages'
                                , let queryId = apSquid ap
                                , queryId `S.member` queryIds
                                ]
                        missingStats <- mapM loadMissingStats filteredPages
                                        :: JSM [Maybe ((QueryId, RunId), MissingAssessmentStats)]
                        return $ LoadRuns rest (M.fromList $ catMaybes missingStats)


  where loadMissingStats :: AssessmentPage -> JSM (Maybe ((QueryId, RunId), MissingAssessmentStats))
        loadMissingStats page = do
            maybeState <- loadLocalModel (fromJust username) (apSquid page)
                        :: JSM (Maybe AssessmentState)
            return $ case maybeState of
                    Nothing -> Nothing
                    Just state -> let missingStats =  pageStats state page
                                  in Just $ ((apSquid page, apRunId page), missingStats)

        newModel = Debug.traceShow ("LoadRuns "<> show files <> " " <> show prevMissingStats) $ m { missingStatsMap = prevMissingStats `M.union` missingStatsMap }


updateModel Noop m =  noEff m


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


-- synch with Main
newtype FormatString = FormatString MisoString
    deriving (Show, Eq)
jsonFormat :: FormatString
jsonFormat = FormatString "json"
jsonLFormat :: FormatString
jsonLFormat = FormatString "jsonl"



-- synch with Main
getAssessmentPageFilePath :: String -> MisoString
getAssessmentPageFilePath pageName =
    ms
    $ "/data/" <> pageName



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

-- sync with Main
fetchJsonL :: forall a. FromJSON a => JSString -> IO (Either FetchJSONError [a])
fetchJsonL url = do
    result <- fetchByteString url
    case result of
        Left err          -> pure $ Left err
        Right byteStr -> pure $ mapM (either (Left . InvalidJSON) Right . eitherDecodeStrict) $ BS.lines byteStr



fetchCbor :: forall a. CBOR.Serialise a => JSString -> IO (Either FetchJSONError [a])
fetchCbor url = do
    result <- fetchByteString url
    case result of
        Left err        -> pure $ Left err
        Right byteStr   -> let hdr ::  CAR.Header
                               (hdr, pages) = CBOR.decodeCborList $ Data.ByteString.Lazy.fromStrict byteStr
                           in pure $ Right pages



fetchByteString :: JSString -> IO (Either FetchJSONError BS.ByteString)
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
    div_ []
           [ p_ [] [a_[href_ "/index.html"][text $ "To Start Page..."]]
           , h1_ [] [text $ (runListTitle username)]
           , ul_ [] $ fmap renderTopics  pages
           ]
  where renderTopics page =
            li_ [] [ h2_ [] [text $ ms $ unpackPageName $ pageName page]
                   , p_ [] [text $ ms $ unpackPageId $ pageId page]
                   , ul_ [] $ (renderGold (pageId page)  : fmap (renderFile (pageId page) page ) filenames)
                   ]

        renderFile :: PageId -> Page -> MisoString -> View Action
        renderFile pageId' page f =
            let fname = T.pack $ System.FilePath.dropExtension $ fromMisoString f
            in li_ [] [
                  p_ [] [
                    a_ [href_ $ toAssessUrl fname (T.pack $ unpackPageId pageId') ] [text $ ms fname]
                    , text $ renderMissingStats $ (pageIdToQueryId pageId', fname) `M.lookup` missingStatsMap
                  ]
              ]

        renderMissingStats :: Maybe MissingAssessmentStats -> MisoString
        renderMissingStats (Just missing) =
            ms $ "  [ missing facets: "<> (show $ numMissingFacetAsessments missing )<> " / missing transitions: "<> (show $ numMissingTransitionAssessments missing <> " ]")
        renderMissingStats Nothing =
            ""

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

