{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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

import qualified Codec.Serialise as CBOR
import qualified Codec.Serialise.Encoding as CBOR
import qualified Codec.CBOR.Write as CBOR

import qualified Data.Binary as B
import Pipes
import qualified Pipes.Prelude as PP
import qualified Control.Concurrent.ForkMap as CM

import Development.GitRev
import Options.Applicative

import Data.MediaWiki.XmlDump (NamespaceId, Format, WikiDoc(..), parseWikiDocs)
import qualified Data.MediaWiki.XmlDump as XmlDump
import Data.MediaWiki.Markup as Markup
import CAR.Types
import CAR.Utils
import Entities

newtype EncodedCbor a = EncodedCbor {getEncodedCbor :: BSL.ByteString}

instance (CBOR.Serialise a) => B.Binary (EncodedCbor a) where
    get = EncodedCbor <$> B.get
    put = B.put . getEncodedCbor

encodedCbor :: CBOR.Serialise a => a -> EncodedCbor a
encodedCbor = EncodedCbor . CBOR.serialise

instance B.Binary NamespaceId
instance B.Binary Format
instance B.Binary XmlDump.PageId
instance B.Binary WikiDoc

commit :: String
commit = $(gitHash)

opts :: Parser (Int, SiteId -> Provenance)
opts =
    (,)
       <$> option auto (short 'j' <> long "jobs" <> metavar "N" <> help "Number of workers" <> value 1)
       <*> prov
  where
    prov = do
        wikiDumpDate <- option str (short 'D' <> long "dump-date" <> metavar "DATE" <> help "Wikipedia dump date" <> value "unknown")
        dataReleaseName <- option str (short 'N' <> long "release-name" <> metavar "NAME" <> help "Data release name" <> value "unknown")
        comments <- fmap unlines $ many $ option str (short 'C' <> long "comments" <> metavar "NAME" <> help "Other comments about data release")
        return (\wikiSite -> Provenance {toolsCommit = commit, ..})

main :: IO ()
main = do
    (workers, prov) <- execParser $ info (helper <*> opts) mempty
    (siteInfo, docs) <- parseWikiDocs <$> BSL.getContents
    let siteId = SiteId $ XmlDump.siteDbName siteInfo

        parsed :: Producer (Either String (EncodedCbor Page)) IO ()
        parsed =
            CM.map (2*workers) workers
                (fmap encodedCbor . toPage defaultConfig siteId)
                (each $ filter isInteresting docs)
        putParsed (Left err) = hPutStrLn stderr $ "\n"<>err
        putParsed (Right page) = BSL.putStr (getEncodedCbor page) >> hPutStr stderr "."

    BSL.putStr $ CBOR.toLazyByteString
        $ CBOR.encode (Header { headerType = PagesFile
                              , provenance = prov siteId
                              })
       <> CBOR.encodeListLenIndef
    runEffect $ parsed >-> PP.mapM_ putParsed
    BSL.putStr $ CBOR.toLazyByteString $ CBOR.encodeBreak

data Config = Config { isCategory :: PageName -> Bool
                     , isDisambiguation :: PageName -> Bool
                     }

defaultConfig :: Config
defaultConfig =
    Config { isCategory = \name -> "Category" `T.isPrefixOf` getPageName name
           , isDisambiguation = \name -> "(disambiguation)" `T.isPrefixOf` getPageName name
           }

isInteresting :: WikiDoc -> Bool
isInteresting WikiDoc{..} = not $
       "Category talk:" `BS.isPrefixOf` docTitle
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
    || "Portal:" `BS.isPrefixOf` docTitle
    || "Module:" `BS.isPrefixOf` docTitle
    || "Draft:" `BS.isPrefixOf` docTitle
    || "Help:" `BS.isPrefixOf` docTitle
    || "Book:" `BS.isPrefixOf` docTitle
    || "TimedText:" `BS.isPrefixOf` docTitle
    || "MediaWiki:" `BS.isPrefixOf` docTitle

toPage :: Config -> SiteId -> WikiDoc -> Either String Page
toPage Config{..} site WikiDoc{..} =
    toPage' <$> Markup.parse (T.unpack $ TE.decodeUtf8 docText)
  where
    toPage' contents =
        --trace (unlines $ map show $ dropRefs contents)
        page
      where
        page = Page { pageName     = name
                    , pageId       = pageId
                    , pageSkeleton = skeleton
                    , pageMetadata = metadata
                    }
        pageId   = pageNameToId site name
        name     = normPageName $ PageName $ TE.decodeUtf8 docTitle
        skeleton = docsToSkeletons site pageId contents
        categories =   map linkTargetId
                     $ filter (isCategory . linkTarget)
                     $ foldMap pageSkeletonLinks skeleton

        pageType
          | isCategory name             = CategoryPage
          | isDisambiguation name       = DisambiguationPage
          | Just _ <- pageRedirect page = RedirectPage
          | otherwise                   = ArticlePage
        metadata =
            PageMetadata { pagemetaType   = pageType
                         , redirectNames  = []
                         , pageCategories = categories
                         }

docsToSkeletons :: SiteId -> PageId -> [Doc] -> [PageSkeleton]
docsToSkeletons siteId thisPage =
      toSkeleton siteId thisPage
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
parseSkeleton =
    fmap (docsToSkeletons (SiteId "Test Site") (PageId "Test Page")) . Markup.parse

isTemplate :: Doc -> Bool
isTemplate (Template{}) = True
isTemplate _            = False

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

normPageName :: PageName -> PageName
normPageName (PageName target) =
    PageName $ normFirst target
  where
    normFirst t = (\(a,b) -> T.toUpper a `T.append` b) $ T.splitAt 1 t

type TemplateTag = Text

resolveTemplate :: Doc -> [Doc]
resolveTemplate (Template tmpl' args)
  | "IPA-" `T.isPrefixOf` tmpl    = []
  | "IPAc-" `T.isPrefixOf` tmpl   = []
  | "lang-" `T.isPrefixOf` tmpl
  , ((Nothing, body):_) <- args   = body
  | "Infobox" `T.isPrefixOf` tmpl = [] -- don't show infoboxes, even with alt

  | Just alt <- lookupNamed "alt" args = alt
  | Just handler <- HM.lookup (T.toCaseFold tmpl) templates
  , Just res <- handler args = concatMap resolveTemplate res
  where tmpl = T.pack $ getAllText tmpl'
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
    , "transl"           .= langTemplate
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

-- | Is a 'Doc' an image element?
isImage :: Doc -> Maybe (T.Text, [Doc])
isImage (InternalLink target parts)
  | Just name <- "file:" `T.stripPrefix` page
  = image name
  | Just name <- "image:" `T.stripPrefix` page
  = image name
  where
    page = T.toCaseFold $ getPageName $ linkTargetPage target
    image name
      | [] <- parts = Just (name, [])
      | otherwise   = Just (name, last parts)
isImage _ = Nothing

-- | A terrible workaround for our inability to robustly parse bold/italics.
dropQuotes :: String -> String
dropQuotes ('\'':xs) =
    case xs of
      '\'':'\'':'\'':'\'':x:rest -> x : dropQuotes rest
      '\'':'\'':x:rest           -> x : dropQuotes rest
      '\'':x:rest                -> x : dropQuotes rest
      x:rest                     -> '\'' : x : dropQuotes rest
      []                         -> '\'' : []
dropQuotes (x:rest) = x : dropQuotes rest
dropQuotes [] = []

-- | We need to make sure we handle cases like,
-- @''[postwar tribunals]''@
toParaBody :: SiteId -> PageId -> Doc -> Maybe [ParaBody]
toParaBody siteId thisPage = go
  where
    go (Text x)        = Just [ParaText $ T.pack x]
    go (Char x)        = Just [ParaText $ T.singleton x]
    go  Bold           = Just []
    go  Italic         = Just []
    go  BoldItalic     = Just []
    go doc@(InternalLink target parts)
      | Just _ <- isImage doc
                       = Nothing
      | otherwise      =
            let linkTarget   = normPageName page
                linkSection  = linkTargetAnchor target
                isSelfLink   = null $ unpackPageName $ linkTargetPage target
                linkTargetId
                  | isSelfLink = thisPage
                  | otherwise  = pageNameToId siteId linkTarget
                linkAnchor   = resolveEntities t
            in Just [ParaLink $ Link {..}]
      where
        page = linkTargetPage target
        t = case parts of
              [anchor] -> T.pack $ getAllText anchor
              _        -> getPageName page
    go (ExternalLink _url (Just anchor))
                       = Just [ParaText $ T.pack anchor]
    go _               = Nothing

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


getPrefix :: (a -> Maybe b) -> [a] -> ([b], [a])
getPrefix f = go []
  where
    go acc [] = (reverse acc, [])
    go acc (x : xs)
      | Just y <- f x = go (y:acc) xs
      | otherwise     = (reverse acc, x:xs)

-- | Collapse consecutive 'ParaText' nodes.
collapseParaBodies :: [ParaBody] -> [ParaBody]
collapseParaBodies = filter (not . isEmptyText) . go
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

nullParaBody :: ParaBody -> Bool
nullParaBody (ParaText t) = T.null $ T.strip t
nullParaBody _            = False

mkParagraph :: [ParaBody] -> Paragraph
mkParagraph bodies = Paragraph (paraBodiesToId bodies) bodies

-- | Does the @[Doc]@ begin with a paragraph? If so, return it and the remaining
-- 'Doc's.
splitParagraph :: SiteId -> PageId -> [Doc] -> Maybe (Paragraph, [Doc])
splitParagraph siteId thisPage docs
  | (bodies@(_:_), rest) <- getPrefix (toParaBody siteId thisPage) docs
  , let bodies' = collapseParaBodies $ concat bodies
  , not $ all nullParaBody bodies'
  = Just (mkParagraph bodies', rest)
  | otherwise
  = Nothing

toSkeleton :: SiteId -> PageId -> [Doc] -> [PageSkeleton]
toSkeleton siteId thisPage = go
  where
    go [] = []
    go docs
      | Just (para, rest) <- splitParagraph siteId thisPage docs
      = Para para : go rest
    go (doc : docs)
      | Just (target, caption) <- isImage doc
      = Image target (go caption) : go docs
    go (Heading lvl title : docs) =
        let (children, docs') = break isParentHeader docs
            isParentHeader (Heading lvl' _) = lvl' <= lvl
            isParentHeader _                = False
            heading = SectionHeading $ resolveEntities $ T.pack $ getAllText title
        in Section heading (sectionHeadingToId heading) (go children) : go docs'
    go (_ : docs)                 = go docs
