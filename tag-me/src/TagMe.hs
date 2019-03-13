{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module TagMe
  ( annotateWithEntityLinks
  , annotateWithEntityLinksConf
  , entityLinkAnnotationsConf
  -- * Environment for tag me communication
  , mkTagMeEnv, TagMeEnv
  -- * Supported TagMe languages
  , Language(..), langEn, langIt, langDe
  -- * Configuring TagMe
  , Token(..)
  , defaultTagMeOptions, TagMeOptions(..)
  -- * Return type
  , TextBody(..), Annotation(..)
  -- * tagMe for long texts that need to be chunked
  , tagLongText
  ) where

import Servant.Client
import Servant.API
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS
import Web.FormUrlEncoded

import GHC.Generics

import qualified Data.Text as T
import Data.Aeson
import Data.Proxy
import Data.List
import Data.String
import Data.Maybe
import Control.Exception


-- ----------------------------------------------
-- TagMe API bindings
-- ----------------------------------------------


newtype Token = Token T.Text
    deriving stock (Show, Eq)
    deriving newtype (FromJSON, ToJSON, IsString, ToHttpApiData)

newtype Language = Language T.Text
    deriving stock (Show, Eq)
    deriving newtype (FromJSON, ToJSON, IsString, ToHttpApiData)

-- ^
langEn :: Language
langEn = Language "en"
langIt :: Language
langIt = Language "it"
langDe :: Language
langDe = Language "de"


tagMeBaseUrl :: BaseUrl
tagMeBaseUrl = BaseUrl Https "tagme.d4science.org" 443 "/tagme"

data TagMePayload = TagMePayload { text :: T.Text, lang :: Language }
    deriving (ToForm, Generic)

type API = "tag" :> QueryParam "gcube-token" Token
                 :> ReqBody '[FormUrlEncoded] TagMePayload
--                  :> QueryParam "lang" Language
                 -- optional  Params
                 :> QueryParam "include_abstract" Bool
                 :> QueryParam "include_categories" Bool
                 :> QueryParam "tweet" Bool
                 :> QueryParam "long_text" Bool
                 :> QueryParam "include_all_spots" Bool  -- always false
--                  :> QueryParam "epsilon" Double -- not really supported by the TagMe api anymore
                 :> Post '[JSON] TagMeResponse

data Annotation = Annotation { spot :: T.Text
                             , start :: Int
                             , end :: Int
                             , linkProbability :: Double
                             , rho :: Double
                             , id :: Int
                             , title :: Maybe T.Text
                             , dbpediaCategories :: Maybe [T.Text]
                             , abstract :: Maybe T.Text
                             }
    deriving (Show)


instance ToJSON Annotation where
    toJSON Annotation{..} = object
        [ "spot" .= spot
        , "start" .= start
        , "end" .= end
        , "link_probability" .= linkProbability
        , "rho" .= rho
        , "id" .= id
        , "title" .= title
        , "dbpedia_categories" .= dbpediaCategories
        , "abstract" .= abstract
        ]

instance FromJSON Annotation where
    parseJSON = withObject "annotation" $ \o ->
        Annotation <$> o .: "spot"
                   <*> o .: "start"
                   <*> o .: "end"
                   <*> o .: "link_probability"
                   <*> o .: "rho"
                   <*> o .: "id"
                   <*> o .:? "title"
                   <*> o .:? "dbpedia_categories"
                   <*> o .:? "abstract"

data TagMeResponse = TagMeResponse {annotations :: [Annotation] }
    deriving Show
instance FromJSON TagMeResponse where
    parseJSON = withObject "response" $ \o ->
        TagMeResponse <$> o .: "annotations"

tagMe :: Token -> T.Text -> Language -> Bool -> Bool -> Bool -> Bool ->  ClientM TagMeResponse
tagMe a txt c d e f g = client (Proxy @API) (Just a) (TagMePayload txt c) (Just d) (Just e) (Just f) (Just g) (Just False)


-- ----------------------------------------------
-- Convert response into sane format
-- ----------------------------------------------

data TextBody = TextPlain !T.Text
              | TextEntityLink !T.Text !Annotation
              deriving (Show)



toTextBodies :: T.Text -> [Annotation] -> [TextBody]
toTextBodies text annotations =
  -- the following code assumes non-overlapping entity link annotations. If annotations overlap, hell breaks loose.
  -- We catch overlaps and exceeding text length with errors.
    let annotations' = sortOn start annotations
        (lastPos, ok) = foldl (\(pos, ok) ann -> ((end ann), ok && pos <= (start ann)) ) (0, True) annotations'

    in if not ok then error $ "Found overlapping annotations, cannot apply this code. Annotations: "<> show annotations'
       else if lastPos > T.length text then error $ "Last annotation offset exceeds text length. Offset "<> show lastPos <> " text length "<> show (T.length text)
       else go 0 text annotations'
  where
    go :: Int -> T.Text -> [Annotation] -> [TextBody]
    go _ "" [] = []
    go _ text [] = [TextPlain text]
    go _ "" anns | not (null anns) = error $ "Non-empty Annotations after end of text: " <> show anns

    go offset text (ann:annotations') | not (T.null text) =
        if (posStart < 0) then (error $ "Annotation beyond text offset "<> show offset<> ". Annotation: "<> show ann)
        else [ TextPlain plainText
             , TextEntityLink linkText ann
             ] <> go (end ann) text' annotations'
      where
        posStart = (start ann) - offset
        posLen = (end ann) - (start ann)
        (plainText, textRest) = T.splitAt posStart text
        (linkText, text') = T.splitAt posLen textRest




tagLongText :: TagMeEnv -> Token -> TagMeOptions -> Int -> Int -> T.Text -> IO [Annotation]
tagLongText env tagMeToken tagMeOptions maxLen  overlapLen txt = do
    anns <- sequence
            [ do anns <- entityLinkAnnotationsConf env tagMeToken t tagMeOptions
                 return [ ann { start = (start ann) + i*maxLen
                              , end = (end ann) + i*maxLen}
                        | ann <- anns
                        ]
            | (i, t) <- zip [0..] $ overlapChunks maxLen overlapLen txt
            ]
    return $ mconcat anns

  where
    overlapChunks :: Int -> Int -> T.Text -> [T.Text]
    overlapChunks k o text =
        [ substring start (start+k+o) text
        | start <- [0, k .. T.length text]
        ]
      where
        substring:: Int -> Int -> T.Text -> T.Text
        substring start end text =
            T.take (end-start) $ T.drop start text


-- ----------------------------------------------
-- Entity Link annotation function
-- ----------------------------------------------

data TagMeOptions = TagMeOptions { inclAbstract :: Bool
                                 , inclCategories :: Bool
                                 , isTweet :: Bool
                                 , isLongText :: Bool
                                 , language :: Language
                                 }
    deriving (Show)

defaultTagMeOptions = TagMeOptions False False False True (Language "en")

annotateWithEntityLinks :: TagMeEnv -> Token -> T.Text -> IO [TextBody]
annotateWithEntityLinks env token text  = annotateWithEntityLinksConf env token text defaultTagMeOptions

-- ^ Run tagme and convert to TextBody
annotateWithEntityLinksConf :: TagMeEnv -> Token -> T.Text -> TagMeOptions -> IO [TextBody]
annotateWithEntityLinksConf env token text opt = do
    anns <- entityLinkAnnotationsConf env token text opt
    return $ toTextBodies text anns

-- ^ just run Tagme and hand back annotations (no TextBody conversion)
entityLinkAnnotationsConf :: TagMeEnv -> Token -> T.Text -> TagMeOptions -> IO [Annotation]
entityLinkAnnotationsConf env token text (TagMeOptions {..}) = run env $ do
    response <- tagMe token text language inclAbstract inclCategories isTweet isLongText
    return $ filter (\a -> not $ isNothing $ title a) $ annotations $ response


newtype TagMeEnv = TagMeEnv ClientEnv

mkTagMeEnv :: Int -> IO TagMeEnv
mkTagMeEnv timeout = do
    let timeout' = responseTimeoutMicro $ seconds timeout
        settings = tlsManagerSettings { managerResponseTimeout = timeout'}
    mgr <- newTlsManagerWith settings
    return $ TagMeEnv $ mkClientEnv mgr tagMeBaseUrl
  where seconds x = x * 1000 * 1000

run :: TagMeEnv -> ClientM a -> IO a
run (TagMeEnv env) action = do
    result <- runClientM action env
    case result of
        Right a -> return a
        Left err -> throwIO err

