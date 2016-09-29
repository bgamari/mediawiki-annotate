{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import Debug.Trace
import Control.Monad
import Data.Either
import Data.Maybe
import Data.Char (isSpace)
import System.IO

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import qualified Data.Binary.Serialise.CBOR as CBOR
import qualified Data.Binary.Serialise.CBOR.Write as CBOR
import Text.Trifecta

import Data.MediaWiki.XmlDump
import Data.MediaWiki.Markup as Markup
import Types

main :: IO ()
main = do
    (namespaces, docs) <- parseWikiDocs <$> BSL.getContents
    let (failed, pages) = partitionEithers $ map (eitherResult . toPage) $ filter isInteresting docs
    BSL.putStr $ CBOR.toLazyByteString $ foldMap CBOR.encode pages
    hPutStrLn stderr "Failures:"
    mapM_ (hPutStrLn stderr . show) failed

eitherResult :: Result a -> Either String a
eitherResult (Success a) = Right a
eitherResult (Failure a) = Left $ show a

isInteresting :: WikiDoc -> Bool
isInteresting WikiDoc{..} = not $
       "#REDIRECT" `BS.isInfixOf` BS.take 20 docText
    || "Talk:" `BS.isPrefixOf` docTitle
    -- || "Portal:" `BS.isPrefixOf` docTitle

toPage :: WikiDoc -> Result Page
toPage WikiDoc{..} =
    toPage' <$> parseByteString (many Markup.doc) mempty docText
  where
    toPage' contents =
        Page { pageName     = PageName docTitle
             , pageSkeleton = toSkeleton $ dropRefs contents
             }

dropRefs :: [Doc] -> [Doc]
dropRefs (XmlOpen "ref" : xs) =
    case dropWhile (not . isClose) xs of
      []   -> []
      _:xs -> dropRefs xs
  where
    isClose (XmlClose "ref") = True
    isClose _                = False
dropRefs (XmlOpenClose "ref" : xs) = dropRefs xs
dropRefs (x:xs) = x : dropRefs xs
dropRefs [] = []

-- | We need to make sure we handle cases like,
-- @''[postwar tribunals]''@
toParaBody :: Doc -> Maybe [ParaBody]
toParaBody (Text x)        = Just [ParaText $ TE.decodeUtf8 x]
toParaBody (Bold xs)       = Just $ concat $ mapMaybe toParaBody xs
toParaBody (Italic xs)     = Just $ concat $ mapMaybe toParaBody xs
toParaBody (BoldItalic xs) = Just $ concat $ mapMaybe toParaBody xs
toParaBody (InternalLink page anchor)
  | PageName page' <- page
  , "File:" `BS.isPrefixOf` page'
  = Nothing
  | otherwise
  = Just [ParaLink page t]
  where t = T.concat $ mapMaybe getText anchor
toParaBody (ExternalLink _url anchor)
  = Just [ParaText $ T.concat $ mapMaybe getText anchor]
toParaBody _ = Nothing

getText :: Doc -> Maybe T.Text
getText (Text x)        = Just $ TE.decodeUtf8 x
getText (Bold xs)       = Just $ T.concat $ mapMaybe getText xs
getText (Italic xs)     = Just $ T.concat $ mapMaybe getText xs
getText (BoldItalic xs) = Just $ T.concat $ mapMaybe getText xs
getText _               = Nothing


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
toSkeleton (Header lvl title : docs) =
    let (children, docs') = break isParentHeader docs
        isParentHeader (Header lvl' _) = lvl' <= lvl
        isParentHeader _               = False
    in Section (TE.decodeUtf8 title) (toSkeleton children) : toSkeleton docs'
toSkeleton (_ : docs)                = toSkeleton docs
