-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric#-}
{-# LANGUAGE DuplicateRecordFields#-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import Miso hiding (go)
import Miso.String hiding (concatMap, filter, length, zip)
import Data.Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import JavaScript.Web.XMLHttpRequest

import Control.Exception
import qualified Data.ByteString as BS
-- import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.List as L
import Data.Maybe
import Data.Time
import Data.Char
import Language.Porter

import qualified Data.Text as T

import JSDOM.URLSearchParams
import JavaScript.Web.Location

import CAR.Types
import Types

import qualified Debug.Trace as Debug

mss :: Show a => a -> MisoString
mss = ms . show


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
prettyTransition SameTransition = "Same Topic"
prettyTransition AppropriateTransition = "Coherent Transition"
prettyTransition CoherentTransition = "Coherent Transition"
prettyTransition SwitchTransition = "Topic Switch"
prettyTransition OfftopicTransition = "ToOfftopic"
prettyTransition ToNonRelTransition = "ToNonRel"
prettyTransition UnsetTransition = "x"


data DisplayConfig = DisplayConfig { displayAssessments :: Bool
                                   , viewOnly :: Bool
                                   }
  deriving (Eq, Show)
defaultDisplayConfig :: DisplayConfig
defaultDisplayConfig = DisplayConfig { displayAssessments = False
                                     , viewOnly = False
                                     }

data ParaSpan = QuerySpan T.Text
              | FacetSpan T.Text
              | PlainSpan T.Text
              | EntitySpan Link
              | QueryEntitySpan Link
              | FacetEntitySpan Link
  deriving (Eq, Show)
type AnnotatedSpans = [ParaSpan]   -- todo inline type

data AssessmentModel =
    AssessmentModel { page :: AssessmentPage
                    , state :: AssessmentState
                    , config :: DisplayConfig
                    , viewCache :: M.Map ParagraphId AnnotatedSpans
                    , timeCache :: UTCTime
                    , stopwords :: S.Set T.Text
                    , username :: T.Text
                    }
    | FileNotFoundErrorModel { filename :: MisoString }
    | ErrorMessageModel { errorMessage :: MisoString
                        , oldModel :: AssessmentModel
                        }
    | LoadingPageModel { maybeStopwords :: Maybe (S.Set T.Text)
                       , maybeUsername :: Maybe T.Text
                       }
  deriving (Eq, Show)




uploadUrl :: JSString
uploadUrl = "/assessment"

-- | Sum type for application events
data Action
  = FetchAssessmentPage String Bool
  | SetAssessmentPage AssessmentPage UTCTime Bool
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
  | FetchedAuthUsername T.Text
  | LoadAssessmentsFromLocalStorage
  | SetState AssessmentState
  deriving (Show)

data StorageTag = LabelTag | FacetTag | NotesTag | HiddenTag | TransitionTag
    deriving (Show, Read, Eq, Enum)

emptyAssessmentModel :: AssessmentModel
emptyAssessmentModel = LoadingPageModel { maybeStopwords = Nothing
                                        , maybeUsername = Nothing
                                        }

noneFacet :: AssessmentFacet
noneFacet =  AssessmentFacet { apHeading=(SectionHeading "OTHER RELEVANT FACET")
                               , apHeadingId=packHeadingId "NONE_OF_THESE"
                               }
introFacet :: AssessmentFacet
introFacet =  AssessmentFacet { apHeading=(SectionHeading "GENERAL/INTRODUCTION")
                               , apHeadingId=packHeadingId "INTRODUCTION"
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


wrapValue :: AssessmentModel -> v -> AnnotationValue v
wrapValue m v =
    Types.AnnotationValue { annotatorId = username m
                            , timeStamp = timeCache m
                            , sessionId = ""
                            , runIds = [apRunId $ page m]
                            , value = v
                            }

unwrapValue :: AnnotationValue v -> v
unwrapValue AnnotationValue {value = v} = v


maxLabel :: Maybe [AnnotationValue FacetValue] -> AssessmentLabel
maxLabel Nothing = UnsetLabel
maxLabel (Just []) = UnsetLabel
maxLabel (Just lst) = L.maximum [label |  AnnotationValue{value=FacetValue{relevance=label}}  <- lst]


saveLocalState :: (ToJSON a, Show a) => StorageTag ->  UserId -> QueryId -> ParagraphId -> a -> JSM ()
saveLocalState tag userId queryId paraId label = do
    let key = storageKey tag userId queryId paraId
        value = label
    putStrLn $ show key <> "   " <> show value
--     setLocalStorage key value

saveLocalTransition :: (ToJSON a, Show a) => StorageTag ->  UserId -> QueryId -> ParagraphId -> ParagraphId -> a -> JSM ()
saveLocalTransition tag userId queryId paraId1 paraId2 label = do
    let key = storageKeyTransition tag userId queryId paraId1 paraId2
        value = label
    putStrLn $ show key <> "   " <> show value
--     setLocalStorage key value



-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model

updateModel (Initialize) m@LoadingPageModel{} = m <# do
    userresponse <- fetchByteString $ getUsernameUrl
    return $ case userresponse of
      Right username -> FetchedAuthUsername $ decodeByteString username
      Left e -> ReportError $ ms $ show e

updateModel (FetchedAuthUsername username) m = m {maybeUsername = Just username } <# do
    stopWordFile <- fetchByteString $ getStopwordUrl
    return $ case stopWordFile of
      Right stopcontent -> LoadStopWordList stopcontent
      Left e -> ReportError $ ms $ show e


updateModel (LoadStopWordList stopwordList) m = newModel <# do
    -- determine page to load
    loc <- getWindowLocation >>= getSearch
    params <- newURLSearchParams loc
    maybeQ <- get params ("q" :: MisoString)
    maybeViewOnly <- get params ("viewOnly" :: MisoString)
                     :: IO (Maybe MisoString)
    let query = fromMaybe "default" maybeQ
        viewOnly' :: Bool
        viewOnly' = Debug.traceShowId $ read $ fromMisoString $ fromMaybe "False" maybeViewOnly
         -- todo Fix FetchAssessmentPage query viewOnly'
    return $ FetchAssessmentPage query viewOnly'
  where newModel =
            let stopwords :: S.Set T.Text
                stopwords = S.fromList $ T.lines $ decodeByteString stopwordList
            in case m of
                LoadingPageModel{} -> m {maybeStopwords = Just stopwords}
                AssessmentModel{} -> m {stopwords = stopwords}
                _ -> m




updateModel (FetchAssessmentPage pageName viewOnly) m = m <# do
    page <- fetchJson $ getAssessmentPageFilePath pageName
    now' <- getCurrentTime
    return $ case page of
      Right p -> SetAssessmentPage p now' viewOnly
      Left e  -> ReportError $ ms $ show e

updateModel (ReportError e) m = noEff $ ErrorMessageModel e m

updateModel (SetAssessmentPage page now' viewOnly) m =
        case m of
            AssessmentModel {stopwords = stopwords, username = username} ->
                    return LoadAssessmentsFromLocalStorage #>
                                   AssessmentModel  { page=page'
                                     , state=emptyAssessmentState
                                     , config = defaultDisplayConfig { viewOnly = viewOnly }
                                     , viewCache = viewCache stopwords
                                     , timeCache = timeCache
                                     , stopwords = stopwords  -- todo load from storage
                                     , username = username
                                     }
            LoadingPageModel {maybeStopwords = Just stopwords
                             , maybeUsername = Just username
                             } ->
                    return LoadAssessmentsFromLocalStorage #>
                                    AssessmentModel  { page=page'
                                     , state=emptyAssessmentState
                                     , config = defaultDisplayConfig  { viewOnly = viewOnly }
                                     , viewCache = viewCache stopwords
                                     , timeCache = timeCache
                                     , stopwords = stopwords  -- todo load from storage
                                     , username = username
                                     }
            _ -> m <# do return $ ReportError $ (ms  ("Unexpected model for SetAssessmentPage "<>  show m))

  where   page' = page{apQueryFacets = facets'}
          facets' = (apQueryFacets $ page)
                  <> [ noneFacet, introFacet
                     ]
          viewCache stopwords = buildViewTable page stopwords
          timeCache = now'

-- Initialization completed. React to user events.

updateModel LoadAssessmentsFromLocalStorage m = noEff m


updateModel (SetState state') m@AssessmentModel{} = noEff $ m { state = state' }



updateModel (SetAssessment userId queryId paraId label) m@AssessmentModel {state=state} =  newModel <# do
    saveLocalState LabelTag userId queryId paraId label
    return Noop
  where newModel =
            let key = (AssessmentKey queryId paraId)
                facetState' :: [AnnotationValue FacetValue]
                facetState' =
                    case key `M.lookup` (facetState state) of
                        Nothing -> [wrapValue m (FacetValue {facet= noneFacet , relevance= label})]
                        Just (lst) -> [ a {value = FacetValue f label}
                                      | a@AnnotationValue{value= (FacetValue f _ )} <- lst
                                      ]

--                         Just (a@AnnotationValue{value = lst}) -> a { value = [ FacetValue f label | FacetValue f r <- lst] }


            in m {state = state{facetState = M.insert key facetState' (facetState state)}}


updateModel (SetFacet userId queryId paraId headingIdStr) m@AssessmentModel {state=state, page=AssessmentPage{apQueryFacets =facetList}} = newModel <# do
    saveLocalState FacetTag userId queryId paraId headingIdStr
    return Noop
  where newModel =
            let key = (AssessmentKey queryId paraId)
                facets' :: [AssessmentFacet]
                facets' =
                    [ f
                     | f@AssessmentFacet {apHeadingId=hid} <- facetList
                     , (unpackHeadingId hid) == (fromMisoString headingIdStr)
                     ]

                facets'' = if L.null facets' then [noneFacet] else facets'
                label = maxLabel $ key `M.lookup` (facetState state)
                facetState' =
                    [wrapValue m (FacetValue {facet= f, relevance= label}) | f <- facets'']
            in m {state = state{facetState = M.insert key facetState' (facetState state)}}




updateModel (SetNotes userId queryId paraId txt) m@AssessmentModel {state=state@AssessmentState{..}} =  newModel <# do
    saveLocalState NotesTag userId queryId paraId txt
    return Noop
  where newModel =
            let value = [ wrapValue m (T.pack $ fromMisoString txt)]
                notesState' = M.insert (AssessmentKey queryId paraId) value notesState
            in m {state = state{notesState = notesState'}}

updateModel (ToggleHidden userId queryId paraId) m@AssessmentModel {state=state@AssessmentState{nonrelevantState=hiddenState}} =  newModel <# do
    saveLocalState HiddenTag userId queryId paraId newState
    return Noop
  where key =  (AssessmentKey queryId paraId)
        oldState = isJust $ key `M.lookup` hiddenState
        newState = not oldState
        hiddenState' = if newState then  M.insert key (wrapValue m ()) hiddenState
                       else M.delete key hiddenState
        newModel = m {state = state{nonrelevantState = hiddenState'}}

updateModel (SetTransitionAssessment userId queryId paraId1 paraId2 label) m@AssessmentModel {state=state@AssessmentState{..}} =  newModel <# do
    saveLocalTransition TransitionTag userId queryId paraId1 paraId2 label
    return Noop
  where newModel =
            let value = wrapValue m label
                transitionLabelState' = M.insert (AssessmentTransitionKey queryId paraId1 paraId2) value transitionLabelState
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

updateModel ClearAssessments m@AssessmentModel{ page=page, config = DisplayConfig {viewOnly=viewOnly}} = m <# do
    now' <- getCurrentTime
    -- remove from browser data
    return $ SetAssessmentPage page now' viewOnly


updateModel DisplayAssessments m@AssessmentModel{ config=c@DisplayConfig {displayAssessments=display}} =
    noEff $ m { config =
                    c {
                        displayAssessments = not display
                      }
              }

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




getUsernameUrl :: MisoString
getUsernameUrl = "/username"

getStopwordUrl :: MisoString
getStopwordUrl = "/inquery-en.txt"

getAssessmentPageFilePath :: String -> MisoString
getAssessmentPageFilePath pageName =
    ms
    $ "/data/" <> pageName <> ".json"



uploadAssessments ::  AssessmentModel -> IO (Either FetchJSONError ())
uploadAssessments m = do
    putStrLn $ "uploadURL " <> (show uploadUrl)
    now' <- getCurrentTime
    resp <- handle onError $ fmap Right $ xhrByteString $ req now'
    case resp of
      Right Response{..}
        | status == 200      -> pure $ Right ()
      Right resp'             -> pure $ Left $ BadResponse (status resp') (contents resp')
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
makeSavedAssessments AssessmentModel{page= page, state = state, username = username} now' =
    SavedAssessments state meta
  where meta = AssessmentMetaData {
           runIds = [apRunId page]
         , annotatorIds = [username]
         , timeStamp = Just now'
         , sessionId = Nothing
        }

makeSavedAssessments m _ =
    error $ "makeSavedAssessments called on model "<> show m


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
      Right resp'             -> pure $ Left $ BadResponse (status resp') (contents resp')
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
viewModel m@AssessmentModel{
            page= AssessmentPage{..}
            , state = AssessmentState { transitionLabelState = transitionState'
                                      , nonrelevantState = hiddenState'
                                      , notesState = notesState'
                                      , facetState = facetState'
                                      }
            , config = DisplayConfig {..}
            , viewCache = viewCache
            , timeCache = timeCache
            , stopwords = _
            , username = username
          } =

    if viewOnly then div_ []
       [ h1_ [] [text $ ms apTitle]
       , p_ [] [text "Query Id: ", text $ ms $ unQueryId apSquid]
       , p_ [] [text "Run: ", text $ ms apRunId]
       , ol_ [] $ paragraphsViewOnly apParagraphs
       ]
    else div_ []
       [ h1_ [] [text $ ms apTitle]
       , p_ [] [text "Query Id: ", text $ ms $ unQueryId apSquid]
       , p_ [] [text "Run: ", text $ ms apRunId]
       , button_ [class_ "btn-sm", onClick ClearAssessments] [text "Clear"]
       , button_ [class_ "hiddenDisplayBtn btn-sm", onClick DisplayAssessments] [text "Show Assessment Data"]
       , textarea_ [readonly_ True, class_ ("assessment-display "<> hiddenDisplay), id_ "assessment-display"] [
                text $  ms $  AesonPretty.encodePretty $  makeSavedAssessments m timeCache
         ]
       , div_ [id_ "toolbar-container"][
           p_ [] [
               button_ [class_ "toolbar-btn", onClick SaveAssessments] [text "Upload"]
            ]
           , div_ [class_ "infopanel toolbar", id_ "toolbar"] $ createInfoPanel m
         ]
       , hr_ []
       , ol_ [] $ paragraphsAndTransitions apParagraphs
       ]
  where
        createInfoPanel :: AssessmentModel -> [View Action]
        createInfoPanel AssessmentModel{
--             page= AssessmentPage{..}
--             state = AssessmentState { transitionLabelState = transitionState'
--                                       , nonrelevantState = hiddenState'
--                                       , notesState = notesState'
--                                       , facetState = facetState'
--                                       }
--             , username = username
          } =
            [ p_ [] [text "Topic: ", text $ ms $ apTitle]
            , p_ [] [text "You logged on as user: ", text $ ms $ username]
            , p_ [] [text "Remaining assessments:"]
            , ul_ [] [
                li_ [] [text $ "Facets: " <> (ms $ show numMissingFacetAsessments )]
                , li_ [] [text $ "Transitions: " <> (ms $ show numMissingTransitionAsessments) ]
              ]
            ]
          where totalParas = S.fromList $ fmap paraId apParagraphs
                visParas =
                    (totalParas `S.difference`)
                    $ S.fromList
                    $ [p | (AssessmentKey {paragraphId=p}, _ ) <- M.toList hiddenState']
                facetAssessments =
                    S.map (\AssessmentKey {paragraphId=p}-> p) $ M.keysSet facetState'
                visTransitionAssessments =
                    S.filter (\(p1,p2)-> p1 `S.member` visParas && p2 `S.member` visParas)
                    $ S.map (\AssessmentTransitionKey {paragraphId1=p1, paragraphId2=p2} -> (p1,p2))
                    $ M.keysSet transitionState'
                numMissingFacetAsessments = length $ visParas `S.difference` facetAssessments
                numMissingTransitionAsessments =
                    let numTransitions = (length visParas)-1
                    in numTransitions - (length visTransitionAssessments)
        createInfoPanel _ = []

        paragraphsAndTransitions :: [Paragraph] -> [View Action]
        paragraphsAndTransitions paragraphs  =
            go paragraphs Nothing
          where
            -- | we iterate over paragraphs, keeping track of the last unhidden para,
            -- | because that is where the next transition is counted from
            -- | We don't add a transition of the there is no previous unhidden para (i.e., Nothing)
            -- | the next unhidden is para2, only if its hidden, then we use the last previous one.b
            go :: [Paragraph] -> Maybe Paragraph -> [View Action]
            go paragraphs' prevPara =
               case paragraphs' of
                 [] -> []
                 para2:rest ->
                    let optTransition =
                            case prevPara of
                            Just para1 ->
                                 [ renderTransition para1 para2]
                            Nothing -> []
                        prevPara' = if (not $ isHidden para2) then Just para2 else prevPara
                    in optTransition
                         <> [renderParagraph para2]
                         <> go rest prevPara'

        paragraphsViewOnly :: [Paragraph] -> [View Action]
        paragraphsViewOnly paragraphs =
            fmap renderParagraphViewOnly paragraphs
        renderParagraphViewOnly :: Paragraph -> View Action
        renderParagraphViewOnly Paragraph{..} =
            -- liKeyed_ (Key ( ms (unpackParagraphId paraId)))
            li_ [class_ "entity-snippet-li"] [
                    p_ [class_ "entity-snippet-li-text"] $ renderParaBodies paraId
                ]




        isHidden Paragraph{paraId = paraId} =
                let assessmentKey = AssessmentKey queryId paraId
                in isJust $ assessmentKey `M.lookup` hiddenState'

        hiddenDisplay = if displayAssessments then "active-display" else "hidden-display"
--         hiddenDisplayBtn = if displayAssessments then "active-display-btn" else "display-btn"
        queryId = apSquid


        mkHidable hidden _key paraId =
            div_[] [
                button_ [class_ ("hider annotate btn btn-sm "<> hideableClass), onClick (ToggleHidden username queryId paraId)] [text hideableText]
            ]
          where hideableClass = if hidden then "active hidable-hidden" else ""
                hideableText = if hidden then "Removed (click to show)" else "Remove"

        mkButtons key paraId =
            div_[] [
            span_ [class_ "btn-group"] [
                    mkButton paraId MustLabel
                  , mkButton paraId ShouldLabel
                  , mkButton paraId CanLabel
                  , mkButton paraId UnsetLabel
                ]
            ]
          where
            currentLabel = maxLabel $ key `M.lookup` facetState'
            mkButton paraId' label =
              let active = if label==currentLabel then "active" else ""
              in button_ [ class_ ("btn btn-sm "<> active)
                         , onClick (SetAssessment username queryId paraId' label) ]
                         [text $ prettyLabel label]
        mkNotesField key paraId =
            let notesValue = [ value_ (ms $ txt) | Just vs <- pure $ key `M.lookup` notesState', let txt = T.unlines $ fmap unwrapValue vs ]
            in div_ [class_ "notes-div"] [
                label_ [] [text "Notes:"
                    , textarea_ ([ class_ "notes-field"
                                , maxlength_ "1000"
                                , wrap_ "true"
                                , cols_ "100"
                                , placeholder_ "This text relevant, because..."
                                , onChange (\str -> SetNotes username queryId paraId str)
                      ]<> notesValue)[]
                ]
            ]

        mkQueryFacetField key paraId =
            let idStr = storageKey TransitionTag username queryId paraId
                facetList :: [AssessmentFacet]
                facetList = apQueryFacets
                selectedFacets =  fmap (facet . unwrapValue) $ fromMaybe [] $ key `M.lookup` facetState'

            in div_ [] [
                label_ [for_ idStr ] [text "Best fitting query facet(s):"
                    , select_ [ class_ "facet-select"
                               , multiple_ True
                               , size_ "8"
                               , id_ idStr
                               , onChange (\str -> SetFacet username queryId paraId str)
                               ]
                        $ fmap (renderFacet selectedFacets) facetList
                    ]
                ]

          where renderFacet ::[AssessmentFacet] -> AssessmentFacet -> View Action
                renderFacet selectedFacets f@AssessmentFacet{..}=
                    let headingId = unpackHeadingId apHeadingId
                        headingText = getSectionHeading apHeading
                        markSelected = if f `elem` selectedFacets then [ selected_ True] else []
                    in option_ ([value_ $ ms $ headingId] <> markSelected) [text $ ms $ headingText ]




        mkTransitionButtons key paraId1 paraId2 =
                    label_ [for_ $ mss key] [text "Topical coherence of transition:"
                        ,div_ [class_ "trans-group", id_ $ mss key] [
                          mkButton SameTransition
                          , mkButton AppropriateTransition
                          , mkButton SwitchTransition
                          , mkButton UnsetTransition
                        ]
                    ]
                  where
                    current = fromMaybe UnsetTransition $ fmap unwrapValue $ key `M.lookup` transitionState'       -- todo fetch state
                    mkButton label =
                      let active = if label==current then "active" else ""
                      in button_ [ class_ ("btn btn-sm "<> active)
                                 , onClick (SetTransitionAssessment username queryId paraId1 paraId2 label) ]
                                 [text $ prettyTransition label]


        renderTransition:: Paragraph -> Paragraph -> View Action
        renderTransition Paragraph{paraId = paraId1} p2@Paragraph{paraId=paraId2} =
            let assessmentKey = AssessmentTransitionKey queryId paraId1 paraId2
                hiddenTransitionClass = if (isHidden p2) then "hidden-transition-annotation" else "displayed-transition-annotation"
            in  span_ [class_ ("transition-annotation annotation " <> hiddenTransitionClass)] [
                    div_ [class_ ("btn-toolbar annotate") ] [
                        mkTransitionButtons assessmentKey paraId1 paraId2
                    ]
                ]
        renderParagraph :: Paragraph -> View Action
        renderParagraph p@Paragraph{..} =
            let assessmentKey = AssessmentKey queryId paraId
                hidden = isHidden p -- fromMaybe False $ assessmentKey `M.lookup` hiddenState
                hiddenStateClass = if hidden then "hidden-panel" else "shown-panel"

            in li_ [class_ "entity-snippet-li"] [
                p_ [] [
                 mkHidable hidden assessmentKey paraId
                , div_ [class_ hiddenStateClass] [
                    section_ [class_ "container"] [
                    div_ [class_ "container-annotate"] [
                            mkNotesField assessmentKey paraId
                            , mkQueryFacetField assessmentKey paraId
                            , label_ [] [text "Relevance for selected facet(s):"] -- Relevance Assessments:"
                            , span_ [class_ "annotation"][ -- data-item  data-query
                                div_ [class_ "btn-toolbar annotate" ] [
                                    mkButtons assessmentKey paraId
                                ]
                            ]
                            , label_ [for_ "paragraph-id"][text "Paragraph Id:"]
                            , p_ [class_ "paragraph-id", id_ "paragraph-id"] [text $ ms $ unpackParagraphId paraId]
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
                    [a_ [ href_ $ toWikiUrl linkTarget] [text $ ms linkAnchor]]
                renderWord (QueryEntitySpan Link{..}) =
                    [span_ [class_ "queryterm-span"]
                       [a_ [ href_ $ toWikiUrl linkTarget] [text $ ms linkAnchor]]
                    ]
                renderWord (FacetEntitySpan Link{..}) =
                    [span_ [class_ "facetterm-span"]
                       [a_ [ href_ $ toWikiUrl linkTarget] [text $ ms linkAnchor]]
                    ]



viewModel LoadingPageModel { .. } = viewErrorMessage $ "Loading Page"
viewModel FileNotFoundErrorModel { .. }= viewErrorMessage $ "File not Found " <> ms filename
viewModel ErrorMessageModel { .. }= viewErrorMessage $ ms errorMessage


toWikiUrl :: PageName -> MisoString
toWikiUrl pagename =
    "https://en.wikipedia.org/wiki/" <> (ms $ unpackPageName pagename)


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
        annotatedTextSpans (ParaLink link) =
             fmap toSpan $ tokenizeStemmer stopwords (linkAnchor link)
           where toSpan :: Term -> ParaSpan
                 toSpan Punct{surface = word} =
                      EntitySpan $ link {linkAnchor = word}
                 toSpan Term{surface = word, stemmed = Nothing} =
                      EntitySpan $ link {linkAnchor = word}
                 toSpan Term{surface = word, stemmed = Just stemmed} =
                      if (stemmed `S.member` queryWords)
                      then QueryEntitySpan $ link {linkAnchor = word}
                      else if  stemmed `S.member` facetWords
                           then FacetEntitySpan $ link {linkAnchor = word}
                           else EntitySpan $ link {linkAnchor = word}

    in M.fromList $ [ (paraId p, spans)
                    | p <- apParagraphs
                    , let spans = foldMap annotatedTextSpans $ paraBody p
                    ]

  where _traceShowPrefix :: Show x => String -> x -> x
        _traceShowPrefix pref x = Debug.traceShow (pref <> ": " <> show x) x

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
        mySplit predicate txt' = go txt'
            where go :: T.Text -> [Term]
                  go "" = []
                  go txt'' =
                      let (word, rest) = T.break predicate txt''
                          (punct, right) = T.span predicate rest
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