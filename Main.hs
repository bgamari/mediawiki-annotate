{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe
import Data.Monoid
import System.IO

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import qualified Data.Binary.Serialise.CBOR as CBOR
import qualified Data.Binary.Serialise.CBOR.Write as CBOR

import Data.MediaWiki.XmlDump
import Data.MediaWiki.Markup as Markup
import Types

main :: IO ()
main = do
    (namespaces, docs) <- parseWikiDocs <$> BSL.getContents
    mapM_ (printResult . toPage) $ filter isInteresting docs

printResult :: Either String Page -> IO ()
printResult (Right page) = do
    BSL.putStr $ CBOR.toLazyByteString $ CBOR.encode page
    hPutStr stderr "."
printResult (Left err) =
    hPutStrLn stderr $ "\n"<>err

isInteresting :: WikiDoc -> Bool
isInteresting WikiDoc{..} = not $
       "#REDIRECT" `BS.isInfixOf` BS.take 20 docText
    || "Talk:" `BS.isPrefixOf` docTitle
    -- || "Portal:" `BS.isPrefixOf` docTitle

toPage :: WikiDoc -> Either String Page
toPage WikiDoc{..} =
    toPage' <$> Markup.parse (T.unpack $ TE.decodeUtf8 docText)
  where
    toPage' contents =
        --trace (unlines $ map show $ dropRefs contents)
        Page { pageName     = PageName $ TE.decodeUtf8 docTitle
             , pageSkeleton = toSkeleton
                            $ map fixTemplate
                            $ dropRefs contents
             }

dropRefs :: [Doc] -> [Doc]
dropRefs (XmlOpen "ref" _ : xs) =
    case dropWhile (not . isClose) xs of
      []   -> []
      _:xs -> dropRefs xs
  where
    isClose (XmlClose "ref") = True
    isClose _                = False
dropRefs (XmlOpenClose "ref" _ : xs) = dropRefs xs
dropRefs (x:xs) = x : dropRefs xs
dropRefs [] = []

fixTemplate :: Doc -> Doc
fixTemplate x@(Template tmpl args) =
    case tmpl of
      "convert"
        | (Nothing, val) : (Nothing, unit) : _ <- args ->
          Text $ getAllText val <> " " <> getAllText unit
      "lang"
        | _ : (Nothing, text) : _ <- args ->
          Text $ getAllText text
      _ -> x
fixTemplate x = x

-- | We need to make sure we handle cases like,
-- @''[postwar tribunals]''@
toParaBody :: Doc -> Maybe [ParaBody]
toParaBody (Text x)        = Just [ParaText $ T.pack x]
toParaBody (Bold xs)       = Just $ concat $ mapMaybe toParaBody xs
toParaBody (Italic xs)     = Just $ concat $ mapMaybe toParaBody xs
toParaBody (BoldItalic xs) = Just $ concat $ mapMaybe toParaBody xs
toParaBody (InternalLink page parts)
  | PageName page' <- page
  , "File:" `T.isPrefixOf` page'
  = Nothing
  | otherwise
  = Just [ParaLink page t]
  where t = case parts of
              [anchor] -> T.pack $ getAllText anchor
              _        -> getPageName page
toParaBody (ExternalLink _url (Just anchor))
  = Just [ParaText $ T.pack anchor]
toParaBody _ = Nothing

getText :: Doc -> Maybe String
getText (Text x)        = Just $ x
getText (Char c)        = Just $ [c]
getText (Bold xs)       = Just $ getAllText xs
getText (Italic xs)     = Just $ getAllText xs
getText (BoldItalic xs) = Just $ getAllText xs
getText _               = Nothing

getAllText :: [Doc] -> String
getAllText = concat . mapMaybe getText


getPrefix :: (a -> Maybe b) -> [a] -> ([b], [a])
getPrefix f = go []
  where
    go acc [] = (reverse acc, [])
    go acc (x : xs)
      | Just y <- f x = go (y:acc) xs
      | otherwise     = (reverse acc, x:xs)

-- | Collapse consecutive 'ParaText' nodes.
toParas :: [ParaBody] -> [ParaBody]
toParas = go
  where
    go [] = []
    go xs
      | (ys@(_:_), xs') <- getPrefix isText xs
      = ParaText (T.concat ys) : go xs'
    go (x:xs) = x : go xs
    isText (ParaText t) = Just t
    isText _ = Nothing

toSkeleton :: [Doc] -> [PageSkeleton]
toSkeleton [] = []
toSkeleton docs
  | (bodies@(_:_), docs') <- getPrefix toParaBody docs =
        Para (toParas $ concat bodies) : toSkeleton docs'
toSkeleton (Heading lvl title : docs) =
    let (children, docs') = break isParentHeader docs
        isParentHeader (Heading lvl' _) = lvl' <= lvl
        isParentHeader _                = False
    in Section (T.pack title) (toSkeleton children) : toSkeleton docs'
toSkeleton (_ : docs)                = toSkeleton docs
