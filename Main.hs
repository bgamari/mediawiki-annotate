{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import Debug.Trace
import Control.Monad
import Data.Either
import Data.Maybe
import Data.Char (isSpace)

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import qualified Data.Binary.Serialise.CBOR as CBOR
import qualified Data.Binary.Serialise.CBOR.Write as CBOR
import Text.Trifecta

import ParseDump
import MediaWiki
import Types

main :: IO ()
main = do
    (namespaces, docs) <- parseWikiDocs <$> BSL.getContents
    let (failed, pages) = partitionEithers $ map (eitherResult . toPage) $ filter isInteresting docs
    mapM_ print $ take 5 $ pages
    BSL.writeFile "out" $ CBOR.toLazyByteString $ foldMap CBOR.encode pages
    print failed

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
    traceShow docTitle
    (toPage' <$> parseByteString (many MediaWiki.doc) mempty docText)
  where
    toPage' contents =
        Page { pageName     = PageName docTitle
             , pageSkeleton = toSkeleton contents
             }

toParaBody :: Doc -> Maybe ParaBody
toParaBody (getText -> Just t)
  | T.all isSpace t = Nothing
  | otherwise       = Just $ ParaText t
toParaBody (InternalLink page anchor)
  | PageName page' <- page
  , "File:" `BS.isPrefixOf` page'
  = Nothing
  | otherwise
  = Just $ ParaLink page t
  where t = T.concat $ mapMaybe getText anchor
toParaBody (ExternalLink _url anchor)
  = Just $ ParaText $ T.concat $ mapMaybe getText anchor
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
        Para (toParas bodies) : toSkeleton docs'
toSkeleton (Header lvl title : docs) =
    let (children, docs') = break isParentHeader docs
        isParentHeader (Header lvl' _) = lvl' <= lvl
        isParentHeader _               = False
        childSkels = toSkeleton children
    in Section (TE.decodeUtf8 title) (toSkeleton children) : toSkeleton docs'
toSkeleton (_ : docs)                = toSkeleton docs
