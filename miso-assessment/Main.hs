-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric#-}
{-# LANGUAGE DuplicateRecordFields#-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import Miso
import Miso.String hiding (concatMap, filter, length)
import Data.Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import JavaScript.Web.XMLHttpRequest

import Control.Exception
import qualified Data.ByteString as BS
-- import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe
import Data.Time
import Data.Char
import Data.String
import Language.Porter

import qualified Data.Text as T

import JSDOM.URLSearchParams
import JavaScript.Web.Location

import CAR.Types
import Types

import qualified Debug.Trace as Debug



prettyLabel :: AssessmentLabel -> MisoString
prettyLabel MustLabel = "Must"
prettyLabel ShouldLabel = "Should"
prettyLabel CanLabel = "Can"
prettyLabel TopicLabel = "Topic"
prettyLabel NonRelLabel = "No"
prettyLabel TrashLabel = "Trash"
prettyLabel DuplicateLabel = "Duplicate"
prettyLabel UnsetLabel = "x"


prettyTransition :: AssessmentTransitionLabel -> MisoString
prettyTransition RedundantTransition = "Redundant"
prettyTransition SameTransition = "Same"
prettyTransition AppropriateTransition = "Coherent"
prettyTransition SwitchTransition = "Switch"
prettyTransition OfftopicTransition = "ToOfftopic"
prettyTransition ToNonRelTransition = "ToNonRel"
prettyTransition UnsetTransition = "x"


data DisplayConfig = DisplayConfig { displayAssessments :: Bool}
  deriving (Eq, Show)
defaultDisplayConfig :: DisplayConfig
defaultDisplayConfig = DisplayConfig {displayAssessments = False}

data ParaSpan = QuerySpan T.Text
              | FacetSpan T.Text
              | PlainSpan T.Text
              | EntitySpan Link
  deriving (Eq, Show)
type AnnotatedSpans = [ParaSpan]   -- todo inline type

data AssessmentModel =
    AssessmentModel { page :: AssessmentPage
                    , state :: AssessmentState
                    , config :: DisplayConfig
                    , viewCache :: M.Map ParagraphId AnnotatedSpans
                    , timeCache :: UTCTime
                    , stopwords :: S.Set T.Text
                    , username :: BS.ByteString
                    }
    | FileNotFoundErrorModel { filename :: MisoString }
    | ErrorMessageModel { errorMessage :: MisoString
                        , oldModel :: AssessmentModel
                        }
    | LoadingPageModel { maybeStopwords :: Maybe (S.Set T.Text)
                       , maybeUsername :: Maybe BS.ByteString
                       }
  deriving (Eq, Show)




uploadUrl :: JSString
uploadUrl = "/assessment"

-- | Sum type for application events
data Action
  = FetchAssessmentPage String
  | SetAssessmentPage AssessmentPage UTCTime
  | ReportError MisoString
  | Initialize
  | SetAssessment UserId QueryId ParagraphId AssessmentLabel
  | SetFacet UserId QueryId ParagraphId MisoString
  | SetNotes UserId QueryId ParagraphId MisoString
  | ToggleHidden UserId QueryId ParagraphId
  | SetTransitionAssessment UserId QueryId ParagraphId ParagraphId AssessmentTransitionLabel
  | Noop
  | FlagSaveSuccess
  | SaveAssessments
  | DisplayAssessments
  | ClearAssessments
  | LoadStopWordList BS.ByteString
  | FetchedAuthUsername BS.ByteString
  deriving (Show)

data StorageTag = LabelTag | FacetTag | HiddenTag | TransitionTag
    deriving (Show, IsString, Read, Eq)

emptyAssessmentModel :: AssessmentModel
emptyAssessmentModel = LoadingPageModel { maybeStopwords = Nothing
                                        , maybeUsername = Nothing
                                        }

noneFacet =  AssessmentFacet { apHeading=(SectionHeading "NONE OF THESE")
                               , apHeadingId=packHeadingId "NONE_OF_THESE"
                               }
-- | Type synonym for an application model
type Model = AssessmentModel


-- | Entry point for a miso application
main :: IO ()
main = startApp App {..}
  where
    initialAction = Initialize -- FetchAssessmentPage "water-distribution" -- initial action to be executed on application load
    model  = emptyAssessmentModel -- initial model
    update = updateModel          -- update function
    view   = viewModel            -- view function
    events = defaultEvents        -- default delegated events
    subs   = []                   -- empty subscription list
    mountPoint = Nothing          -- mount point for application (Nothing defaults to 'body')


saveLocalState :: (ToJSON a, Show a) => StorageTag ->  UserId -> QueryId -> ParagraphId -> a -> JSM ()
saveLocalState tag userId queryId paraId label = do
    let key = storageKey tag userId queryId paraId
        value = label
    putStrLn $ show key <> "   " <> show value
    setLocalStorage key value

saveLocalTransition :: (ToJSON a, Show a) => StorageTag ->  UserId -> QueryId -> ParagraphId -> ParagraphId -> a -> JSM ()
saveLocalTransition tag userId queryId paraId1 paraId2 label = do
    let key = storageKeyTransition tag userId queryId paraId1 paraId2
        value = label
    putStrLn $ show key <> "   " <> show value
    setLocalStorage key value



-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model

updateModel (Initialize) m@LoadingPageModel{} = m <# do
    userresponse <- fetchByteString $ getUsernameUrl
    return $ case userresponse of
      Right authUsername -> FetchedAuthUsername authUsername
      Left e -> ReportError $ ms $ show e

updateModel (FetchedAuthUsername authUsername) m = m {maybeUsername = Just authUsername } <# do
    stopWordFile <- fetchByteString $ getStopwordUrl
    return $ case stopWordFile of
      Right stopcontent -> LoadStopWordList stopcontent
      Left e -> ReportError $ ms $ show e


updateModel (LoadStopWordList stopwordList) m = newModel <# do
    -- determine page to load
    loc <- getWindowLocation >>= getSearch
    params <- newURLSearchParams loc
    maybeQ <- get params ("q" :: MisoString)
    let query = fromMaybe "default" maybeQ
    return $ FetchAssessmentPage query
  where newModel =
            let stopwords :: S.Set T.Text
                stopwords = S.fromList $ T.lines $ decodeByteString stopwordList
            in case m of
                m@LoadingPageModel{} -> m {maybeStopwords = Just stopwords}
                m@AssessmentModel{} -> m {stopwords = stopwords}
                m@ErrorMessageModel{} -> m




updateModel (FetchAssessmentPage pageName) m = m <# do
    page <- fetchJson $ getAssessmentPageFilePath pageName
    now' <- getCurrentTime
    return $ case page of
      Right p -> SetAssessmentPage p now'
      Left e  -> ReportError $ ms $ show e

updateModel (ReportError e) m = noEff $ ErrorMessageModel e m

updateModel (SetAssessmentPage page now') m =
        case m of
            AssessmentModel {stopwords = stopwords, username = username} ->
                    noEff $ AssessmentModel  { page=page'
                                     , state=emptyAssessmentState
                                     , config = defaultDisplayConfig
                                     , viewCache = viewCache stopwords
                                     , timeCache = timeCache
                                     , stopwords = stopwords  -- todo load from storage
                                     , username = username
                                     }
            LoadingPageModel {maybeStopwords = Just stopwords
                             , maybeUsername = Just authUsername
                             } ->
                    noEff $ AssessmentModel  { page=page'
                                     , state=emptyAssessmentState
                                     , config = defaultDisplayConfig
                                     , viewCache = viewCache stopwords
                                     , timeCache = timeCache
                                     , stopwords = stopwords  -- todo load from storage
                                     , username = authUsername
                                     }
            _ -> m <# do return $ ReportError $ (ms  ("Unexpected model for SetAssessmentPage "<>  show m))

  where   page' = page{apQueryFacets = facets'}
          facets' = (apQueryFacets $ page)
                  <> [ noneFacet
                     ]
          viewCache stopwords = buildViewTable page stopwords
          timeCache = now'
-- Initialization completed. React to user events.



updateModel (SetAssessment userId queryId paraId label) m@AssessmentModel {state=state@AssessmentState{..}} =  newModel <# do
    saveLocalState "label" userId queryId paraId label
    return Noop
  where newModel =
            let labelState' = M.insert (AssessmentKey userId queryId paraId) label labelState
            in m {state = state{labelState = labelState'}}


updateModel (SetFacet userId queryId paraId headingIdStr) m@AssessmentModel {state=state@AssessmentState{..}, page=AssessmentPage{apQueryFacets =facetList}} =
    newModel <# do
        saveLocalState "facet" userId queryId paraId headingIdStr
        return Noop
  where newModel =
            let facetState' =
                    case [ f
                         | f@AssessmentFacet {apHeadingId=hid} <- facetList
                         , (unpackHeadingId hid) == (fromMisoString headingIdStr)
                         ] of
                    (facet:_) -> M.insert (AssessmentKey userId queryId paraId) facet facetState
                    otherwise -> M.delete (AssessmentKey userId queryId paraId) facetState
            in m {state = state{ facetState = facetState'}}

updateModel (SetNotes userId queryId paraId txt) m@AssessmentModel {state=state@AssessmentState{..}} =  newModel <# do
    saveLocalState "notes" userId queryId paraId txt
    return Noop
  where newModel =
            let notesState' = M.insert (AssessmentKey userId queryId paraId) (T.pack $ fromMisoString txt) notesState
            in m {state = state{notesState = notesState'}}

updateModel (ToggleHidden userId queryId paraId) m@AssessmentModel {state=state@AssessmentState{..}} =  newModel <# do
    saveLocalState "hidden" userId queryId paraId newState
    return Noop
  where newModel = m {state = state{hiddenState = hiddenState'}}
        oldState = fromMaybe False $ M.lookup  (AssessmentKey userId queryId paraId) hiddenState
        newState = not oldState
        hiddenState' = M.insert (AssessmentKey userId queryId paraId) (newState) hiddenState

updateModel (SetTransitionAssessment userId queryId paraId1 paraId2 label) m@AssessmentModel {state=state@AssessmentState{..}} =  newModel <# do
    saveLocalTransition "transition" userId queryId paraId1 paraId2 label
    return Noop
  where newModel =
            let transitionLabelState' = M.insert (AssessmentTransitionKey userId queryId paraId1 paraId2) label transitionLabelState
            in m {state = state{transitionLabelState = transitionLabelState'}}


updateModel Noop m = noEff m

updateModel SaveAssessments m = m <# do
    res <- uploadAssessments m
    return $ case res of
      Right () -> FlagSaveSuccess
      Left e  -> ReportError $ ms $ show e

updateModel FlagSaveSuccess m@AssessmentModel{page=AssessmentPage{apSquid=queryId}} = m <# do
    alert $ "Uploaded annotations for page " <> (ms $ unQueryId queryId)
    return Noop

updateModel ClearAssessments m@AssessmentModel{ page=page} = m <# do
    now' <- getCurrentTime
    -- remove from browser data
    return $ SetAssessmentPage page now'


updateModel DisplayAssessments m@AssessmentModel{ config=c@DisplayConfig {displayAssessments=display}} =
    noEff $ m { config =
                    c {
                        displayAssessments = not display
                      }
              }


updateModel (SetAssessmentPage x1 x2) m = m <# do
    return $ ReportError $ ms ("unreachable: updateModel SetAssessmentPage "<> show x1 <>" "<> show x2 <> " " <> show m)

updateModel x m = m <# do
    return $ ReportError $ ms ("Unhandled case for updateModel "<> show x <> " " <> show m)









storageKey :: StorageTag -> UserId -> QueryId -> ParagraphId -> MisoString
storageKey tag userId queryId paraId =
    (ms $ show tag) <> "-" <> (ms $ userId) <> "-" <> (ms $ unQueryId queryId) <> "-" <> (ms $ unpackParagraphId paraId)

storageKeyTransition :: StorageTag -> UserId -> QueryId -> ParagraphId -> ParagraphId -> MisoString
storageKeyTransition tag userId queryId paraId1 paraId2 =
    (ms $ show tag) <> "-"
    <> (ms $ userId) <> "-"
    <> (ms $ unQueryId queryId) <> "-"
    <> (ms $ unpackParagraphId paraId1)  <> "-"
    <> (ms $ unpackParagraphId paraId2)

-- updateModel Home m = m <# do
--                h <- windowInnerHeight
--                return $ UpdateModelWithWindow h
-- updateModel (UpdateModelWithWindow h) m = noEff (h)
-- updateModel SayHelloWorld m = m <# do
--   putStrLn "Hello World" >> pure NoOp


getUsernameUrl :: MisoString
getUsernameUrl = "/username"

getStopwordUrl :: MisoString
getStopwordUrl = "/inquery-en.txt"

getAssessmentPageFilePath :: String -> MisoString
getAssessmentPageFilePath pageName =
    ms
    $ "/data/" <> pageName <> ".json"

--     $ "http://trec-car2.cs.unh.edu:8080/data/" <> pageName <> ".json"
--     $ "http://localhost:8000/data/" <> pageName <> ".json"

uploadAssessments ::  AssessmentModel -> IO (Either FetchJSONError ())
uploadAssessments m = do
    putStrLn $ "uploadURL " <> (show uploadUrl)
    now' <- getCurrentTime
    resp <- handle onError $ fmap Right $ xhrByteString $ req now'
    case resp of
      Right (Response{..})
        | status == 200      -> pure $ Right ()
      Right resp             -> pure $ Left $ BadResponse (status resp) (contents resp)
      Left err               -> pure $ Left $ XHRFailed err
  where
    onError :: XHRError -> IO (Either XHRError (Response BS.ByteString))
    onError = pure . Left

    req now' = Request { reqMethod = POST
                  , reqURI = uploadUrl
                  , reqLogin = Nothing
                  , reqHeaders = []
                  , reqWithCredentials = False
                  , reqData = StringData $ ms $  Data.Aeson.encode $  makeSavedAssessments m now'
                  }

makeSavedAssessments :: AssessmentModel -> UTCTime -> SavedAssessments
makeSavedAssessments m@AssessmentModel{page= page, state = state} now' =
    SavedAssessments state meta
  where meta = AssessmentMetaData {
           assessmentRuns = [AssessmentRun { runId = apRunId page
                                           , squid = apSquid page}]
         , userId = defaultUser
         , timeStamp = now'
        }



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
decodeByteString bs = Data.Text.Encoding.decodeUtf8 bs

-- ------------- Presentation ----------------------

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel m@AssessmentModel{
            page= AssessmentPage{..}
            , state = s@AssessmentState { labelState = labelState
                                      , transitionLabelState = transitionState
                                      , hiddenState = hiddenState
                                      }
            , config = c@DisplayConfig {..}
            , viewCache = viewCache
            , timeCache = timeCache
            , stopwords = _
            , username = authUsername
          } =

    div_ []
       [ h1_ [] [text $ ms apTitle]
       , p_ [] [text "Query Id: ", text $ ms $ unQueryId apSquid]
       , p_ [] [text "Run: ", text $ ms apRunId]
       , div_ [class_ "infopanel"] $ createInfoPanel m
       , button_ [onClick SaveAssessments] [text "Upload"]
       , button_ [onClick DisplayAssessments, class_ hiddenDisplayBtn] [text "Display"]
       , button_ [onClick ClearAssessments] [text "Clear"]
       , textarea_ [readonly_ True, class_ ("assessment-display "<> hiddenDisplay), id_ "assessment-display"] [
            text $  ms $  AesonPretty.encodePretty $  makeSavedAssessments m timeCache
       ]
       , hr_ []
       , ol_ [] $ paragraphsAndTransitions apParagraphs

       ]
  where
        createInfoPanel :: AssessmentModel -> [View Action]
        createInfoPanel m@AssessmentModel{
            page= AssessmentPage{..}
            , state = s@AssessmentState { labelState = labelState'
                                      , transitionLabelState = transitionState'
                                      , hiddenState = hiddenState'
                                      , facetState = facetState'
                                      }
            , username = authUsername
          } =
            [ p_ [] [text "You logged on as user: ", text $ ms $ decodeByteString authUsername]
            , p_ [] [text "Remaining assessments on this page:"]
            , ul_ [] [
                li_ [] [text $ "facets: " <> (ms $ show numMissingFacetAsessments )]
                , li_ [] [text $ "relevance: " <> (ms $ show numMissingLabelAsessments) ]
                , li_ [] [text $ "transitions: " <> (ms $ show numMissingTransitionAsessments) ]
              ]
            ]
          where totalParas = S.fromList $ fmap paraId apParagraphs
                visParas =
                    (totalParas `S.difference`)
                    $ S.fromList
                    $ [p | (AssessmentKey {paragraphId=p},True)<-  M.toList hiddenState']
                facetAssessments =
                    S.map (\(AssessmentKey {paragraphId=p})-> p) $ M.keysSet facetState'
                labelAssessments :: S.Set ParagraphId
                labelAssessments =
                    S.map (\(AssessmentKey {paragraphId=p})-> p) $ M.keysSet labelState'
                visTransitionAssessments =
                    S.filter (\(p1,p2)-> p1 `S.member` visParas && p2 `S.member` visParas)
                    $ S.map (\(AssessmentTransitionKey {paragraphId1=p1, paragraphId2=p2})-> (p1,p2))
                    $ M.keysSet transitionState'
                numMissingFacetAsessments = length $ visParas `S.difference` facetAssessments
                numMissingLabelAsessments = length $ visParas `S.difference` labelAssessments
                numMissingTransitionAsessments =
                    let numTransitions = (length visParas)-1
                    in numTransitions - (length visTransitionAssessments)
        createInfoPanel _ = []

        paragraphsAndTransitions :: [Paragraph] -> [View Action]
        paragraphsAndTransitions apParagraphs  =
            go apParagraphs Nothing
          where
            -- | we iterate over paragraphs, keeping track of the last unhidden para,
            -- | because that is where the next transition is counted from
            -- | We don't add a transition of the there is no previous unhidden para (i.e., Nothing)
            -- | the next unhidden is para2, only if its hidden, then we use the last previous one.b
            go :: [Paragraph] -> Maybe Paragraph -> [View Action]
            go apParagraphs prevPara =
               case apParagraphs of
                 [] -> []
                 para2:rest ->
                    let optTransition =
                            case prevPara of
                            Just para1 ->
                                 [ renderTransition para1 para2]
                            Nothing -> []
                        prevPara' = (if (not $ isHidden para2) then Just para2 else prevPara)
                    in optTransition
                         <> [renderParagraph para2]
                         <> go rest prevPara'


        isHidden Paragraph{paraId = paraId} =
                let assessmentKey = (AssessmentKey defaultUser queryId paraId)
                in fromMaybe False $ assessmentKey `M.lookup` hiddenState

        hiddenDisplay = if displayAssessments then "active-display" else "hidden-display"
        hiddenDisplayBtn = if displayAssessments then "active-display-btn" else "display-btn"
        queryId = apSquid

-- todo delete
--         facetMap = M.fromList [ (hid, facet)  |  facet@AssessmentFacet{apHeadingId=hid} <- apQueryFacets]

        mkHidable className _key paraId =
            div_[] [
                button_ [class_ ("hider annotate btn btn-sm "<> className), onClick (ToggleHidden defaultUser queryId paraId)] [text "Hide from Article"]
            ]

        mkButtons key paraId =
            div_[] [
            label_ [for_ "relevance"] [text ""] -- Relevance Assessments:"
            ,span_ [class_ "btn-group", id_ "relevance"] [         --  , role_ "toolbar"
                    mkButton paraId MustLabel
                  , mkButton paraId ShouldLabel
                  , mkButton paraId CanLabel
                  , mkButton paraId TopicLabel
                  , mkButton paraId NonRelLabel
                  , mkButton paraId TrashLabel
                  , mkButton paraId DuplicateLabel
                  , mkButton paraId UnsetLabel
                ]
            ]
          where
            current = fromMaybe UnsetLabel $ key `M.lookup` labelState
            mkButton paraId label =
              let active = if label==current then "active" else ""
              in button_ [ class_ ("btn btn-sm "<> active)
                         , onClick (SetAssessment defaultUser queryId paraId label) ]
                         [text $ prettyLabel label]
        mkNotesField _key paraId =
            div_ [class_ "notes-div"] [
                label_ [] [text "Notes:"
                    , textarea_ [ class_ "notes-field"
                                , maxlength_ "1000"
                                , wrap_ "true"
                                , cols_ "100"
                                , placeholder_ "This text relevant, because..."
                                , onChange (\str -> SetNotes defaultUser queryId paraId str)
                      ][]
                ]
            ]

        mkQueryFacetField _key paraId =
            let idStr = storageKey "transition" defaultUser queryId paraId
                facetList :: [AssessmentFacet]
                facetList = apQueryFacets
            in div_ [] [
                label_ [for_ idStr ] [text "Best fitting query facet:"
                    , (select_ [ class_ "facet-select"
                               , multiple_ True
                               , id_ idStr
                               , onChange (\str -> SetFacet defaultUser queryId paraId str)
                               ]
                        $ [ option_ [ class_ "facet-option", value_ $ ""] [text $ "Please select"]]
                        <> (fmap renderFacet facetList)
                       )
                    ]
                ]

          where renderFacet :: AssessmentFacet -> View Action
                renderFacet AssessmentFacet{..}=
                    let headingId = unpackHeadingId $  apHeadingId
                        headingText = getSectionHeading $ apHeading
                    in option_ [value_ $ ms $ headingId] [text $ ms $ headingText ]




        mkTransitionButtons key paraId1 paraId2 =
                    label_ [] [text "Transition Quality:"
                        ,div_ [class_ "trans-group"] [
                            mkButton RedundantTransition
                          , mkButton SameTransition
                          , mkButton AppropriateTransition
                          , mkButton SwitchTransition
                          , mkButton OfftopicTransition
                          , mkButton ToNonRelTransition
                          , mkButton UnsetTransition
                        ]
                    ]
                  where
                    current = fromMaybe UnsetTransition $ key `M.lookup` transitionState       -- todo fetch state
                    mkButton label =
                      let active = if label==current then "active" else ""
                      in button_ [ class_ ("btn btn-sm "<> active)
                                 , onClick (SetTransitionAssessment defaultUser queryId paraId1 paraId2 label) ]
                                 [text $ prettyTransition label]


        renderTransition:: Paragraph -> Paragraph -> View Action
        renderTransition Paragraph{paraId = paraId1} p2@Paragraph{paraId=paraId2} =
            let assessmentKey = (AssessmentTransitionKey defaultUser queryId paraId1 paraId2)
--                 idStr = storageKey "transition" defaultUser queryId paraId1
                hiddenTransitionClass = if (isHidden p2) then "hidden-transition-annotation" else "displayed-transition-annotation"
            in  span_ [class_ ("transition-annotation annotation " <> hiddenTransitionClass)] [
                    div_ [class_ ("btn-toolbar annotate") ] [
                        mkTransitionButtons assessmentKey paraId1 paraId2
                    ]
                ]
        renderParagraph :: Paragraph -> View Action
        renderParagraph p@Paragraph{..} =
            let assessmentKey = (AssessmentKey defaultUser queryId paraId)
                hidden = isHidden p -- fromMaybe False $ assessmentKey `M.lookup` hiddenState
                hiddenStateClass = if hidden then "hidden-panel" else "shown-panel"
                hidableClass = if hidden then "active hidable-hidden" else ""
--                 hidableClass = if isHidden then "hidable-hidden" else "hidable-shown"
            in li_ [class_ ("entity-snippet-li")] [
                p_ [] [
                 mkHidable hidableClass assessmentKey paraId
                , div_ [class_ hiddenStateClass] [
                    section_ [class_ "container"] [
                    div_ [class_ "container-annotate"] [
                            mkNotesField assessmentKey paraId
                            , mkQueryFacetField assessmentKey paraId
                            ,  span_ [class_ "annotation"][ -- data-item  data-query
                                div_ [class_ "btn-toolbar annotate" ] [ -- data_ann role_ "toolbar"
                                    mkButtons assessmentKey paraId
                                ]
                            ]
                            , p_ [class_ "paragraph-id"] [text $ ms $ unpackParagraphId paraId]
                    ], div_ [class_ "container-content"][
                             p_ [class_ "entity-snippet-li-text"] $ renderParaBodies paraId
                    ]
                    ]
                ]
                ]
            ]
        renderParaBodies :: ParagraphId -> [View Action]
        renderParaBodies paraId  =
            let annotatedTextsSpans = fromJust $ paraId `M.lookup` viewCache
            in foldMap renderWord annotatedTextsSpans
          where renderWord :: (ParaSpan) -> [View Action]
                renderWord (QuerySpan str) =
                    [span_ [class_ "queryterm-span"] [text $ ms str]]
                renderWord (FacetSpan str) =
                    [span_ [class_ "facetterm-span"] [text $ ms str]]
                renderWord (PlainSpan str) =
                    [text $ ms str]
                renderWord (EntitySpan Link{..}) =
                    [a_ [ href_ $ ms $ unpackPageName linkTarget] [text $ ms linkAnchor]]



viewModel LoadingPageModel { .. } = viewErrorMessage $ "Loading Page"
viewModel FileNotFoundErrorModel { .. }= viewErrorMessage $ "File not Found " <> ms filename
viewModel ErrorMessageModel { .. }= viewErrorMessage $ ms errorMessage


viewErrorMessage :: MisoString -> View Action
viewErrorMessage msg = div_ []
    [ h1_[] [text $ msg ],
      p_[] [ a_ [ href_ "/"] [text "Back to start..."]]
    ]


buildViewTable :: AssessmentPage -> S.Set T.Text -> M.Map ParagraphId AnnotatedSpans
buildViewTable AssessmentPage{..} stopwords =
    let facetStrings = fmap (getSectionHeading . apHeading) apQueryFacets
        queryWords :: S.Set T.Text
        queryWords = S.fromList
                   $ mapMaybe termToStem
                   $ tokenizeStemmer stopwords apTitle
        facetWords :: S.Set T.Text
        facetWords = S.fromList
                   $ mapMaybe termToStem
                   $ concatMap ( tokenizeStemmer stopwords) facetStrings

        annotatedTextSpans :: ParaBody -> AnnotatedSpans
        annotatedTextSpans (ParaText txt) =
             fmap toSpan $ tokenizeStemmer stopwords txt
           where toSpan :: Term -> ParaSpan
                 toSpan Punct{surface = word} =
                      PlainSpan word
                 toSpan Term{surface = word, stemmed = Nothing} =
                      PlainSpan word
                 toSpan Term{surface = word, stemmed = Just stemmed} =
                      if (stemmed `S.member` queryWords)
                      then QuerySpan word
                      else if  stemmed `S.member` facetWords
                           then FacetSpan word
                           else PlainSpan word
        annotatedTextSpans (ParaLink link) = [EntitySpan link]

    in M.fromList $ [ (paraId p, spans)
                    | p <- apParagraphs
                    , let spans = foldMap annotatedTextSpans $ paraBody p
                    ]

  where traceShowPrefix :: Show x => String -> x -> x
        traceShowPrefix pref x = Debug.traceShow (pref <> ": " <> show x) x

data Term = Term { surface :: T.Text, stemmed :: Maybe T.Text }
          | Punct { surface :: T.Text }
        deriving (Eq, Show)
termToStem :: Term -> Maybe T.Text
termToStem Punct {} = Nothing
termToStem Term {stemmed = value} = value


tokenizeStemmer :: S.Set T.Text -> T.Text ->  [Term]
tokenizeStemmer stopwords txt =
    mySplit isSplit txt
  where isSplit :: Char -> Bool
        isSplit c =
            isSpace c || isSymbol c || isPunctuation c

        mySplit :: (Char -> Bool) -> T.Text -> [Term]
        mySplit pred txt = go txt
            where go :: T.Text -> [Term]
                  go "" = []
                  go txt =
                      let (word, rest) = T.break pred txt
                          (punct, right) = T.span pred rest
                      in if T.length word > 0
                           then [Term{ surface = word, stemmed = myStem word}, Punct punct] <> go right
                           else [Punct punct] <> go right

        myStem :: T.Text -> Maybe T.Text
        myStem word =
            let lowerWord = T.toCaseFold word
                stemmed = stem $ T.unpack $ lowerWord
            in  if Prelude.length stemmed < 3 || lowerWord `S.member` stopwords
                    then Nothing
                    else (Just $ T.pack stemmed)