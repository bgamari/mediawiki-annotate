{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isSpace)
import Data.List (intersperse, isPrefixOf)
import Data.Maybe
import Data.Monoid
import System.IO

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM

import qualified Data.Binary.Serialise.CBOR as CBOR
import qualified Data.Binary.Serialise.CBOR.Write as CBOR

import Data.MediaWiki.XmlDump
import Data.MediaWiki.Markup as Markup
import CAR.Types

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
    || "Special:" `BS.isPrefixOf` docTitle
    || "User talk:" `BS.isPrefixOf` docTitle
    -- || "Portal:" `BS.isPrefixOf` docTitle

toPage :: WikiDoc -> Either String Page
toPage WikiDoc{..} =
    toPage' <$> Markup.parse (T.unpack $ TE.decodeUtf8 docText)
  where
    toPage' contents =
        --trace (unlines $ map show $ dropRefs contents)
        Page { pageName     = PageName $ TE.decodeUtf8 docTitle
             , pageSkeleton = toSkeleton
                            $ concatMap resolveTemplate
                            $ filter (not . isComment)
                            $ dropXml "super"
                            $ dropXml "sub"
                            $ dropXml "ref"
                            $ contents
             }

isComment :: Doc -> Bool
isComment (Comment{}) = True
isComment _           = False

dropXml :: String -> [Doc] -> [Doc]
dropXml tag (XmlOpen tag' _ : xs)
  | tag == tag'
  = case dropWhile (not . isClose) xs of
      []   -> []
      _:xs -> dropXml tag xs
  where
    isClose (XmlClose tag'') = tag == tag''
    isClose _                = False
dropXml tag (XmlOpenClose tag' _ : xs)
  | tag == tag'
  = dropXml tag xs
dropXml tag (x:xs) = x : dropXml tag xs
dropXml _   [] = []

resolveTemplate :: Doc -> [Doc]
resolveTemplate (Template tmpl args)
  | Just alt <- lookupNamed "alt" args = alt
  | "IPA-" `isPrefixOf` tmpl = []
  | "IPAc-" `isPrefixOf` tmpl = []
  | "lang-" `isPrefixOf` tmpl
  , ((Nothing, body):_) <- args = body

  | Just handler <- HM.lookup (T.toCaseFold $ T.pack tmpl) templates
  , Just res <- handler args = res
resolveTemplate x = [x]

templates :: HM.HashMap T.Text ([(Maybe String, [Doc])] -> Maybe [Doc])
templates = HM.fromList $
    map (.= listTemplate)
    [ "bulleted list", "blist", "bulleted", "ulist", "unordered list"
    , "unbulleted list", "ubl", "ubt", "ublist", "unbullet"
    , "plainlist"
    , "ordered list"
    , "hlist"
    , "flatlist"
    ] ++
    map (.= convertTemplate)
    [ "cvt"
    , "convert"
    ] ++
    [ "as of"            .= asOfTemplate
    , "lang"             .= langTemplate
    , "time ago"         .= timeAgoTemplate
    , "cardinal to word" .= simpleTemplate
    , "number to word"   .= simpleTemplate
    , "ordinal to word"  .= simpleTemplate
    , "nowrap"           .= simpleTemplate
    , "small"            .= simpleTemplate
    , "smaller"          .= simpleTemplate
    , "midsize"          .= simpleTemplate
    , "larger"           .= simpleTemplate
    , "big"              .= simpleTemplate
    , "large"            .= simpleTemplate
    , "huge"             .= simpleTemplate
    , "resize"           .= simpleTemplate
    , "mvar"             .= simpleTemplate
    , "format price"     .= simpleTemplate
    , "inflation"        .= inflationTemplate
    , "citation needed"  .= dropTemplate
    ]
  where
    a .= b = (a,b)
    justText :: String -> Maybe [Doc]
    justText x = Just [Text x]

    dropTemplate _ = Nothing

    listTemplate args =
        Just $ intersperse (Text ", ") $ concat $ mapMaybe isUnnamed args

    convertTemplate ((Nothing, val) : (Nothing, unit) : _) =
        justText $ getAllText val <> " " <> getAllText unit
    convertTemplate _ = Nothing

    langTemplate (_ : (Nothing, body) : _) = Just body
    langTemplate _ = Nothing

    asOfTemplate args =
        case mapMaybe isUnnamed args of
          [year, month, day] -> Just $ unwords' [[leader], day, month, year]
          [year, month]      -> Just $ unwords' [[leader], month, year]
          [year]             -> Just $ unwords' [[leader], year]
          _                  -> Nothing
      where
        unwords' = concat . intersperse [Char ' ']
        leader
          | since, lowercase = Text "since"
          | since            = Text "Since"
          | lowercase        = Text "as of"
          | otherwise        = Text "As of"
        since = lookupNamed "since" args == Just [Text "y"]
        lowercase = lookupNamed "lc" args == Just [Text "y"]

    simpleTemplate ((Nothing, val) : _) = Just $ concatMap resolveTemplate val
    simpleTemplate _                    = Nothing

    timeAgoTemplate ((Nothing, [Text time]) : _)
      | '-':rest <- trimmed = justText $ rest ++ " ago"
      | '+':rest <- trimmed = justText $ rest ++ "'s time'"
      | otherwise           = justText $ "on " ++ time
      where
        trimmed = dropWhile isSpace time
    timeAgoTemplate _ = Nothing

    inflationTemplate (_ : (Nothing, [Text amount]) : _) = justText amount
    inflationTemplate _ = Nothing

lookupNamed :: String -> [(Maybe String, [Doc])] -> Maybe [Doc]
lookupNamed key = listToMaybe . mapMaybe (isNamed key)

isNamed :: String -> (Maybe String, [Doc]) -> Maybe [Doc]
isNamed key (Just key', val)
  | key == key'  = Just val
isNamed _   _    = Nothing

isUnnamed :: (Maybe String, [Doc]) -> Maybe [Doc]
isUnnamed (Nothing, val) = Just val
isUnnamed _              = Nothing

-- | We need to make sure we handle cases like,
-- @''[postwar tribunals]''@
toParaBody :: Doc -> Maybe [ParaBody]
toParaBody (Text x)        = Just [ParaText $ T.pack x]
toParaBody (Char x)        = Just [ParaText $ T.singleton x]
toParaBody (Bold xs)       = Just $ concat $ mapMaybe toParaBody xs
toParaBody (Italic xs)     = Just $ concat $ mapMaybe toParaBody xs
toParaBody (BoldItalic xs) = Just $ concat $ mapMaybe toParaBody xs
toParaBody (InternalLink page parts)
  | PageName page' <- page
  , "file:" `T.isPrefixOf` T.toCaseFold page'
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
toParaBodies :: [ParaBody] -> [ParaBody]
toParaBodies = go
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
        let bodies' = toParaBodies $ concat bodies
        in Para (Paragraph (toParagraphId bodies') bodies') : toSkeleton docs'
toSkeleton (Heading lvl title : docs) =
    let (children, docs') = break isParentHeader docs
        isParentHeader (Heading lvl' _) = lvl' <= lvl
        isParentHeader _                = False
        heading = SectionHeading $ T.pack title
    in Section heading (toSkeleton children) : toSkeleton docs'
toSkeleton (_ : docs)                = toSkeleton docs
