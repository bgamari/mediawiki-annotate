{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module CAR.Import.ConfigFile where

import GHC.Generics
import Data.Maybe
import Data.List
import Control.Applicative
import Control.Monad

import qualified Text.Trifecta as Tri
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Aeson

import Data.MediaWiki.Markup hiding (PageName(..))
import CAR.Types (PageName(..))
import CAR.Import.Utils
import CAR.Import.Templates
import CAR.Import

data ResolutionPart = TRText String
                    | TRPositionalArg Int
                    | TRNamedArg TemplateTag

parseTemplateResolution :: Tri.Parser ResolutionPart
parseTemplateResolution = positionalArg <|> namedArg <|> text
  where
    positionalArg = do
        void $ Tri.text "{{#"
        n <- fromIntegral <$> Tri.natural
        void $ Tri.text "}}"
        pure $ TRPositionalArg n
    namedArg = do
        void $ Tri.text "{{"
        name <- many Tri.alphaNum
        void $ Tri.text "}}"
        pure $ TRNamedArg $ T.pack name
    text = do
        c <- Tri.anyChar
        cs <- many $ Tri.noneOf "{"
        pure $ TRText $ c:cs

listTemplate :: TemplateHandler
listTemplate args =
    Just $ map (List [Bulleted]) (mapMaybe isUnnamed args)

runTemplateResolution :: TemplateResolution -> TemplateHandler
runTemplateResolution ListTemplate args = listTemplate args
runTemplateResolution (ViaResolution res) args =
    concat <$> mapM resolve res
  where
    (posArgs, namedArgs) = partition (isNothing . fst) args

    resolve :: ResolutionPart -> Maybe [Doc]
    resolve (TRText t) = Just [Text t]
    resolve (TRPositionalArg n)
      | (_, x):_ <- drop n posArgs = Just x
      | otherwise                  = Nothing
    resolve (TRNamedArg n) = lookup (Just n) namedArgs

data TemplateResolution = ListTemplate
                        | ViaResolution [ResolutionPart]

instance FromJSON TemplateResolution where
    parseJSON = withText "template resolution" $ \t ->
      case t of
        "&list" -> pure ListTemplate
        _ ->
          case Tri.parseString (many parseTemplateResolution) mempty (T.unpack t) of
            Tri.Success a -> pure $ ViaResolution a
            Tri.Failure e -> fail $ show e

instance ToJSON TemplateResolution where
    toJSON ListTemplate       = String "&list"
    toJSON (ViaResolution rs) = toJSON $ concatMap showResolutionPart rs

showResolutionPart :: ResolutionPart -> String
showResolutionPart (TRText s) = s
showResolutionPart (TRPositionalArg n) = "{{#"++show n++"}}"
showResolutionPart (TRNamedArg n) = "{{"++T.unpack n++"}}"

data ConfigFile = ConfigFile { disambiguationTemplates :: [TemplateTag]
                             , templateResolutions :: HM.HashMap TemplateTag TemplateResolution
                             , infoboxTemplates :: HS.HashSet TemplateTag
                             }
                deriving (Generic)
instance FromJSON ConfigFile
instance ToJSON ConfigFile

configFileToConfig :: T.Text -> ConfigFile -> Config
configFileToConfig categoryNamespaceName c =
    Config { isCategory = \name -> categoryNamespaceName `T.isPrefixOf` getPageName name
           , isDisambiguation = \_ -> any (`elem` disambiguationTemplates c) . mapMaybe isTemplate
           , isInfoboxTemplate = 
               let infoboxTemplates' = map T.toCaseFold $ HS.toList $ infoboxTemplates c
               in \templateName ->   
                let templateName' = T.toCaseFold templateName
                in any (`T.isPrefixOf` templateName') infoboxTemplates'
                     -- Old Infobox handling: tag `HS.member` infoboxTemplates c
           , resolveTemplate = \tag args -> do
                 res <- HM.lookup tag (templateResolutions c)
                 runTemplateResolution res args
           }
