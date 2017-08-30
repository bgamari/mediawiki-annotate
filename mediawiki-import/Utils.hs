module Utils where

import Data.Maybe
import qualified Data.Text as T

import Data.MediaWiki.Markup

normPageName :: PageName -> PageName
normPageName (PageName target) =
    PageName $ normFirst target
  where
    normFirst t = (\(a,b) -> T.toUpper a `T.append` b) $ T.splitAt 1 t

type TemplateTag = T.Text

isTemplate :: Doc -> Maybe TemplateTag
isTemplate (Template tag _) = Just $ T.toCaseFold $ T.pack $ getAllText tag
isTemplate _ = Nothing

isComment :: Doc -> Bool
isComment (Comment{}) = True
isComment _           = False

takeXml :: T.Text -> [Doc] -> [Doc]
takeXml tag (XmlTag tag' _ children : xs)
  | T.toCaseFold tag == T.toCaseFold tag'
  = children ++ takeXml tag xs
takeXml tag (doc : xs)
  | isXmlOpenClose tag doc
  = takeXml tag xs
takeXml tag (x:xs) = x : takeXml tag xs
takeXml _   [] = []

dropXml :: T.Text -> [Doc] -> [Doc]
dropXml = replaceXml []

replaceXml :: [Doc] -> T.Text -> [Doc] -> [Doc]
replaceXml sub tag (doc : xs)
  | isXml tag doc          = sub ++ replaceXml sub tag xs
  | isXmlOpenClose tag doc = sub ++ replaceXml sub tag xs
replaceXml sub tag (x:xs) = x : replaceXml sub tag xs
replaceXml _   _   [] = []

isXmlOpenClose, isXml :: T.Text -> Doc -> Bool
isXmlOpenClose tag (XmlOpenClose tag' _)   =
    T.toCaseFold tag == T.toCaseFold tag'
isXmlOpenClose _   _                     = False

isXml tag (XmlTag tag' _ _) =
    T.toCaseFold tag == T.toCaseFold tag'
isXml _   _              = False

getText :: Doc -> Maybe String
getText (Text x)        = Just x
getText (Char c)        = Just [c]
getText  Bold           = Just ""
getText  Italic         = Just ""
getText  BoldItalic     = Just ""
getText (InternalLink target [])  = Just $ T.unpack $ getPageName $ linkTargetPage target
getText (InternalLink _ (xs:_))   = Just $ getAllText xs
getText (ExternalLink _ (Just s)) = Just s
getText _               = Nothing

getAllText :: [Doc] -> String
getAllText = concat . mapMaybe getText
