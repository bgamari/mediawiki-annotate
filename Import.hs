{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

import Data.Char (isSpace)
import Data.List (intersperse)
import Data.Maybe
import Data.Monoid
import System.IO

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM

import qualified Data.Binary.Serialise.CBOR as CBOR
import qualified Data.Binary.Serialise.CBOR.Write as CBOR

import qualified Data.Binary as B
import Pipes
import qualified Pipes.Prelude as PP
import qualified ConcurrentMap as CM

import Data.MediaWiki.XmlDump (NamespaceId, Format, WikiDoc(..), parseWikiDocs)
import qualified Data.MediaWiki.XmlDump as XmlDump
import Data.MediaWiki.Markup as Markup
import CAR.Types
import Entities

workers :: Int
workers = 16

newtype EncodedCbor a = EncodedCbor {getEncodedCbor :: BSL.ByteString}

instance (CBOR.Serialise a) => B.Binary (EncodedCbor a) where
    get = EncodedCbor <$> B.get
    put = B.put . getEncodedCbor

encodedCbor :: CBOR.Serialise a => a -> EncodedCbor a
encodedCbor = EncodedCbor . CBOR.toLazyByteString . CBOR.encode

instance B.Binary NamespaceId
instance B.Binary Format
instance B.Binary PageId
instance B.Binary XmlDump.PageId
instance B.Binary WikiDoc

main :: IO ()
main = do
    (namespaces, docs) <- parseWikiDocs <$> BSL.getContents
    let parsed :: Producer (Either String (EncodedCbor Page)) IO ()
        parsed =
            CM.map (2*workers) workers
                (fmap encodedCbor . toPage)
                (each $ filter isInteresting docs)
        putParsed (Left err) = hPutStrLn stderr $ "\n"<>err
        putParsed (Right page) = BSL.putStr (getEncodedCbor page) >> hPutStr stderr "."
    runEffect $ parsed >-> PP.mapM_ putParsed

isInteresting :: WikiDoc -> Bool
isInteresting WikiDoc{..} = not $
       "#REDIRECT" `BS.isInfixOf` BS.take 20 docText
    || "Category:" `BS.isPrefixOf` docTitle
    || "Category talk:" `BS.isPrefixOf` docTitle
    || "Talk:" `BS.isPrefixOf` docTitle
    || "File:" `BS.isPrefixOf` docTitle
    || "File talk:" `BS.isPrefixOf` docTitle
    || "Special:" `BS.isPrefixOf` docTitle
    || "User:" `BS.isPrefixOf` docTitle
    || "User talk:" `BS.isPrefixOf` docTitle
    || "Wikipedia talk:" `BS.isPrefixOf` docTitle
    || "Wikipedia:" `BS.isPrefixOf` docTitle
    || "Template:" `BS.isPrefixOf` docTitle
    || "Template talk:" `BS.isPrefixOf` docTitle
    -- || "Portal:" `BS.isPrefixOf` docTitle

toPage :: WikiDoc -> Either String Page
toPage WikiDoc{..} =
    toPage' <$> Markup.parse (T.unpack $ TE.decodeUtf8 docText)
  where
    toPage' contents =
        --trace (unlines $ map show $ dropRefs contents)
        Page { pageName     = name
             , pageId       = pageNameToId name
             , pageSkeleton = docsToSkeletons contents
             }
      where name = PageName $ TE.decodeUtf8 docTitle

docsToSkeletons :: [Doc] -> [PageSkeleton]
docsToSkeletons =
      toSkeleton
    . filter (not . isTemplate) -- drop unknown templates here so they don't
                                -- break up paragraphs
    . concatMap resolveTemplate
    . filter (not . isComment)
    . takeXml "code"
    . takeXml "s"
    . takeXml "math"
    . dropXml "gallery"
    . dropXml "sup"
    . dropXml "sub"
    . dropXml "ref"
    . dropXml "timeline"

-- | For testing.
parseSkeleton :: String -> Either String [PageSkeleton]
parseSkeleton = fmap docsToSkeletons . Markup.parse

isTemplate :: Doc -> Bool
isTemplate (Template{}) = True
isTemplate _            = False

isComment :: Doc -> Bool
isComment (Comment{}) = True
isComment _           = False

takeXml :: String -> [Doc] -> [Doc]
takeXml tag (XmlOpen tag' _ : xs)
  | tag == tag'
  = case break isClose xs of
      (body, [])     -> body
      (body, _:rest) -> body ++ takeXml tag rest
  where
    isClose (XmlClose tag'') = tag == tag''
    isClose _                = False
takeXml tag (XmlOpenClose tag' _ : xs)
  | tag == tag'
  = takeXml tag xs
takeXml tag (x:xs) = x : takeXml tag xs
takeXml _   [] = []

dropXml :: String -> [Doc] -> [Doc]
dropXml = replaceXml []

replaceXml :: [Doc] -> String -> [Doc] -> [Doc]
replaceXml sub tag (XmlOpen tag' _ : xs)
  | tag == tag'
  = case dropWhile (not . isClose) xs of
      []   -> sub
      _:xs -> sub ++ dropXml tag xs
  where
    isClose (XmlClose tag'') = tag == tag''
    isClose _                = False
replaceXml sub tag (XmlOpenClose tag' _ : xs)
  | tag == tag'
  = replaceXml sub tag xs
replaceXml sub tag (x:xs) = x : replaceXml sub tag xs
replaceXml _   _   [] = []

type TemplateTag = Text

resolveTemplate :: Doc -> [Doc]
resolveTemplate (Template tmpl args)
  | "IPA-" `T.isPrefixOf` tmpl    = []
  | "IPAc-" `T.isPrefixOf` tmpl   = []
  | "lang-" `T.isPrefixOf` tmpl
  , ((Nothing, body):_) <- args   = body
  | "Infobox" `T.isPrefixOf` tmpl = [] -- don't show infoboxes, even with alt

  | Just alt <- lookupNamed "alt" args = alt
  | Just handler <- HM.lookup (T.toCaseFold tmpl) templates
  , Just res <- handler args = concatMap resolveTemplate res
resolveTemplate x = [x]

type TemplateHandler = [(Maybe Text, [Doc])] -> Maybe [Doc]

templates :: HM.HashMap TemplateTag TemplateHandler
templates = HM.fromList $
    -- Lists
    map (.= listTemplate)
    [ "bulleted list", "blist", "bulleted", "ulist", "unordered list"
    , "unbulleted list", "ubl", "ubt", "ublist", "unbullet"
    , "plainlist"
    , "ordered list"
    , "hlist"
    , "flatlist"
    ] ++
    -- Text styling
    map (.= simpleTemplate)
    [ "small" , "smaller" , "midsize" , "larger" , "big" , "large" , "huge" , "resize"
    , "smallcaps", "sc1", "smallcaps2", "sc2", "sc", "allcaps", "caps", "nocaps"
    ] ++
    -- Unit conversion
    map (.= convertTemplate)
    [ "cvt"
    , "convert"
    ] ++
    -- Other
    [ "as of"            .= asOfTemplate
    , "lang"             .= langTemplate
    , "rtl-lang"         .= langTemplate
    , "time ago"         .= timeAgoTemplate
    , "angbr"            .= sandwichTemplate [Text "⟨"] [Text "⟩"]
    , "linktext"         .= simpleTemplate
    , "cardinal to word" .= simpleTemplate
    , "number to word"   .= simpleTemplate
    , "ordinal to word"  .= simpleTemplate
    , "nowrap"           .= simpleTemplate
    , "mvar"             .= simpleTemplate
    , "format price"     .= simpleTemplate
    , "visible anchor"   .= simpleTemplate
    , "quotation"        .= simpleTemplate
    , "cquote"           .= simpleTemplate
    , "inflation"        .= inflationTemplate
    , "citation needed"  .= dropTemplate
    , "respell"          .= dropTemplate
    , "ref"              .= dropTemplate
    , "refn"             .= dropTemplate
    , "r"                .= dropTemplate  -- synonym for ref
    , "zh"               .= dropTemplate  -- "Chinese: "
    , "sfn"              .= dropTemplate  -- shortened footnote
    ]
  where
    a .= b = (a,b)
    justText :: String -> Maybe [Doc]
    justText x = Just [Text x]

    dropTemplate _ = Nothing

    listTemplate args =
        Just $ concat $ [Text " "] : intersperse [Text ", "] (mapMaybe isUnnamed args)

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

    sandwichTemplate :: [Doc] -> [Doc] -> TemplateHandler
    sandwichTemplate before after [(Nothing, xs)] = Just $ before ++ xs ++ after
    sandwichTemplate _      _     _               = Nothing

    inflationTemplate (_ : (Nothing, [Text amount]) : _) = justText amount
    inflationTemplate _ = Nothing

lookupNamed :: TemplateTag -> [(Maybe Text, [Doc])] -> Maybe [Doc]
lookupNamed key = listToMaybe . mapMaybe (isNamed key)

isNamed :: TemplateTag -> (Maybe Text, [Doc]) -> Maybe [Doc]
isNamed key (Just key', val)
  | key == key'  = Just val
isNamed _   _    = Nothing

isUnnamed :: (Maybe Text, [Doc]) -> Maybe [Doc]
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
  | PageName page' <- page
  , "image:" `T.isPrefixOf` T.toCaseFold page'
  = Nothing
  | otherwise
  = Just [ParaLink page (resolveEntities t)]
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
getText (InternalLink _ (xs:_))   = Just $ getAllText xs
getText (ExternalLink _ (Just s)) = Just s
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
toParaBodies = filter (not . isEmptyText) . go
  where
    go [] = []
    go xs
      | (ys@(_:_), xs') <- getPrefix isText xs
      = ParaText (resolveEntities $ T.concat ys) : go xs'
    go (x:xs) = x : go xs

    isText (ParaText t) = Just t
    isText _ = Nothing

    isEmptyText (ParaText t) = T.null t
    isEmptyText _            = False

toSkeleton :: [Doc] -> [PageSkeleton]
toSkeleton [] = []
toSkeleton docs
  | (bodies@(_:_), docs') <- getPrefix toParaBody docs
  , let bodies' = toParaBodies $ concat bodies
  , not $ null bodies'
  = Para (Paragraph (paraBodiesToId bodies') bodies') : toSkeleton docs'
  where
toSkeleton (Heading lvl title : docs) =
    let (children, docs') = break isParentHeader docs
        isParentHeader (Heading lvl' _) = lvl' <= lvl
        isParentHeader _                = False
        heading = SectionHeading $ resolveEntities $ T.pack $ getAllText title
    in Section heading (sectionHeadingToId heading) (toSkeleton children) : toSkeleton docs'
toSkeleton (_ : docs)                = toSkeleton docs
