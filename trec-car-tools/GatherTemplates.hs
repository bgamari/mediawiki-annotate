{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe

import Text.Show.Pretty (ppShow)
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Text.Megaparsec
import Text.Megaparsec.Expr

import qualified Data.MediaWiki.XmlDump as XmlDump
import Data.MediaWiki.Markup

main :: IO ()
main = do
    (_, docs) <- XmlDump.parseWikiDocs <$> BSL.getContents
    let templates = HM.fromList $ mapMaybe toTemplate docs
    mapM_ (putStrLn . ppShow) $ HM.toList templates

toTemplate :: XmlDump.WikiDoc -> Maybe (T.Text, [Doc])
toTemplate doc
  | Just name <- "Template:" `T.stripPrefix` T.decodeUtf8 (XmlDump.docTitle doc) =
    let Right content = parse (BS.unpack $ XmlDump.docText doc)
    in Just (name, mapMaybe dropNoinclude content)
  | otherwise = Nothing

data TemplateArg = Positional !Int
                 | Named !T.Text

data TExpr =
           -- 10
             ENegate TExpr
           | ETimesPow10 TExpr TExpr
           -- 9
           | EExp TExpr
           | ELn TExpr
           | EAbs TExpr
           | ESqrt TExpr
           | ETrunc TExpr
           | EFloor TExpr
           | ECeil TExpr
           | ESin TExpr
           | ECos TExpr
           | ETan TExpr
           | EASin TExpr
           | EACos TExpr
           | EATan TExpr
           | ENot TExpr
           -- 8
           | EPow TExpr TExpr
           -- 7
           | EMult TExpr TExpr
           | EDiv TExpr TExpr
           | EMod TExpr TExpr
           -- 6
           | EPlus TExpr TExpr
           | EMinus TExpr TExpr
           -- 5
           | ERound TExpr TExpr
           -- 4
           | EEq TExpr TExpr
           | ENEq TExpr TExpr
           | ELT TExpr TExpr
           | EGT TExpr TExpr
           | ELTE TExpr TExpr
           | EGTE TExpr TExpr
           -- 3
           | EAnd TExpr TExpr
           -- 2
           | EOr TExpr TExpr
           -- n.a.
           | EPos TExpr
           | ENum !Double
           | EE
           | EPi
           | EParens TExpr
           | EArg TemplateArg TExpr

parseExpr :: [Doc] -> Expr
parseExpr = go []
  where
    go accum (TemplateArg name parts : xs) = go (EArg _ _ : accum) xs
    go accum (TText t) = feed

data TemplateBody = TIf TemplateBody TemplateBody TemplateBody
                  | TIfEq TemplateBody TemplateBody TemplateBody TemplateBody
                  | TSwitch TemplateBody [(TemplateBody, TemplateBody)] (Maybe TemplateBody)
                  | TExpr TExpr
                  | TInvoke TemplateFunction [TemplateBody]

                  | TArg TemplateArg TemplateBody
                  | TXmlTag T.Text [(String, String)] [TemplateBody]
                  | TText !T.Text
                  | TInternalLink LinkTarget [[TemplateBody]]
                  | TTemplate [TemplateBody] [(Maybe T.Text, [Doc])]
                  | THeading Int [TemplateBody]

data TemplateFunction = TTime

docToTemplateBody :: Doc -> Maybe TemplateBody
docToTemplateBody (Text s)                    = Just $ TText $ T.pack s
docToTemplateBody (Char c)                    = Just $ TText $ T.singleton c
docToTemplateBody (Comment _)                 = Nothing
docToTemplateBody (Heading s docs)            = Just $ THeading n $ docsToTemplateBodies docs
docToTemplateBody (InternalLink target docss) = Just $ TInternalLink target $ map docsToTemplateBodies docss
docToTemplateBody (XmlTag tag _ _)
  | T.toCaseFold tag == "noinclude"           = Nothing
docToTemplateBody (XmlTag tag attrs body)     = Just $ TXmlTag tag attrs (docsToTemplateBodies body)
docToTemplateBody (MagicWord word parts)      = case T.toCaseFold word of
                                                  "ifexpr"
                                                    | cond:true:false:[] -> TIf (TExpr $ parseExpr cond) true false
                                                    | cond:true:[]       -> TIf (TExpr $ parseExpr cond) true (TText "")
                                                  "if"
                                                    | cond:true:false:[] -> TIf (TExpr $ parseExpr cond) true false
                                                    | cond:true:[]       -> TIf (TExpr $ parseExpr cond) true (TText "")
docToTemplateBody _                           = Nothing

docsToTemplateBodies :: [Doc] -> [TemplateBody]
docsToTemplateBodies = mapMaybe toTemplateBody
