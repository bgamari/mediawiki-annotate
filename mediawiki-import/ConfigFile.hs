{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module ConfigFile where

import GHC.Generics
import Data.Maybe
import Data.List
import Control.Applicative
import Control.Monad

import qualified Text.Trifecta as Tri
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Data.Aeson

import Data.MediaWiki.Markup
import Utils
import Templates
import Import

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

runTemplateResolution :: TemplateResolution -> TemplateHandler
runTemplateResolution (TemplateResolution res) args =
    concat <$> mapM resolve res
  where
    (posArgs, namedArgs) = partition (isJust . fst) args

    resolve :: ResolutionPart -> Maybe [Doc]
    resolve (TRText t) = Just [Text t]
    resolve (TRPositionalArg n)
      | (_, x):_ <- drop n posArgs = Just x
      | otherwise                  = Nothing
    resolve (TRNamedArg n) = lookup (Just n) namedArgs

newtype TemplateResolution = TemplateResolution [ResolutionPart]

instance FromJSON TemplateResolution where
    parseJSON = withText "template resolution" $ \t ->
      case Tri.parseString (many parseTemplateResolution) mempty (T.unpack t) of
        Tri.Success a -> pure $ TemplateResolution a
        Tri.Failure e -> fail $ show e

instance ToJSON TemplateResolution where
    toJSON (TemplateResolution rs) = toJSON $ concatMap showResolutionPart rs

showResolutionPart :: ResolutionPart -> String
showResolutionPart (TRText s) = s
showResolutionPart (TRPositionalArg n) = "{{#"++show n++"}}"
showResolutionPart (TRNamedArg n) = "{{"++T.unpack n++"}}"

data ConfigFile = ConfigFile { disambiguationTemplates :: [TemplateTag]
                             , templateResolutions :: HM.HashMap TemplateTag TemplateResolution
                             }
                deriving (Generic)
instance FromJSON ConfigFile
instance ToJSON ConfigFile

configFileToConfig :: T.Text -> ConfigFile -> Config
configFileToConfig categoryNamespaceName c =
    Config { isCategory = \name -> categoryNamespaceName `T.isPrefixOf` getPageName name
           , isDisambiguation = \_ -> any (`elem` disambiguationTemplates c) . mapMaybe isTemplate
           , resolveTemplate = \tag args -> do
                 res <- HM.lookup tag (templateResolutions c)
                 runTemplateResolution res args
           }
