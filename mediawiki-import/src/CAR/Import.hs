{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}

module CAR.Import
    ( Config(..)
    , defaultConfig
    , toPage
    , parseSkeleton
    ) where

import Data.Maybe

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Data.MediaWiki.XmlDump (WikiDoc(..))
import Data.MediaWiki.Markup as Markup
import CAR.Types hiding (pageNameToId)
import CAR.Utils hiding (pageRedirect)

import CAR.Import.Entities
import CAR.Import.Templates
import CAR.Import.Utils
import Network.URI
import Data.Char (ord, chr)
import qualified Data.ByteString.Short as SBS

import qualified Data.SmallUtf8 as Utf8

data Config = Config { isCategory :: PageName -> Bool
                     , isDisambiguation :: PageName -> [Doc] -> Bool
                     , isInfoboxTemplate :: TemplateTag -> Bool
                     , resolveTemplate :: TemplateTag -> TemplateHandler
                     }

defaultConfig :: Config
defaultConfig =
    Config { isCategory = \name -> "Category" `T.isPrefixOf` getPageName name
           , isDisambiguation = \name _ -> "(disambiguation)" `T.isPrefixOf` getPageName name
           , isInfoboxTemplate = (== "infobox settlement")
           , resolveTemplate = defaultTemplateHandler
           }

pageNameToId :: SiteId -> PageName -> PageId
pageNameToId (SiteId s) (PageName n) =
    PageId
    $ Utf8.unsafeFromShortByteString
    $ urlEncodeText
    $ T.unpack s ++ ":" ++ T.unpack n
  where urlEncodeText :: String -> SBS.ShortByteString
        urlEncodeText = SBS.pack . map (fromIntegral . ord) . escapeURIString isAllowedInURI


toPage :: Config -> SiteId -> WikiDoc -> Either String Page
toPage config@Config{..} site WikiDoc{..} =
    toPage' <$> Markup.parse (T.unpack $ TE.decodeUtf8 docText)
  where
    toPage' contents =
        --trace (unlines $ map show $ dropRefs contents)
        page
      where
        page = Page { pageName     = name
                    , pageId       = pageId
                    , pageSkeleton = skeleton
                    , pageType     = pageType
                    , pageMetadata = metadata
                    }
        pageId   = pageNameToId site name
        name     = normPageName $ PageName $ TE.decodeUtf8 docTitle
        skeleton = docsToSkeletons config site pageId contents
        categories =  filter (isCategory . linkTarget)
                     $ foldMap pageSkeletonLinks skeleton

        pageType
          | isCategory name                 = CategoryPage
          | isDisambiguation name contents  = DisambiguationPage
          | Just l <- pageRedirect page     = RedirectPage l
          | otherwise                       = ArticlePage
        metadata =
            setMetadata _CategoryNames (map linkTarget categories)
            $ setMetadata _CategoryIds (map linkTargetId categories)
            $ emptyPageMetadata

-- | Identify the target of a redirect page.
--
-- In English redirect pages begin with a paragraph starting with @#redirect@. However,
-- to be langauge-agnostic we instead just look for any page beginning with a word starting
-- with a hash sign, followed by a link.
pageRedirect :: Page -> Maybe Link
pageRedirect (Page {pageSkeleton=Para (Paragraph _ (ParaText t : ParaLink l : _)) : _})
  | Just word <- T.pack "#" `T.stripPrefix` T.toCaseFold (T.strip t)
  , not $ T.null word
  = Just l
pageRedirect _ = Nothing

docsToSkeletons :: Config
                -> SiteId -> PageId -> [Doc] -> [PageSkeleton]
docsToSkeletons config siteId thisPage =
      toSkeleton config siteId thisPage
      -- drop unknown templates here so they don't break up paragraphs
    . filter (not . isUnknownTemplate)
    . concatMap (runTemplateHandler $ resolveTemplate config)
    . filter (not . isComment)
    . takeXml "code"
    . takeXml "s"
    . takeXml "math"
    . dropXml "gallery"
    . dropXml "sup"
    . dropXml "sub"
    . dropXml "ref"
    . dropXml "timeline"
  where
    isUnknownTemplate doc
      | Just tag <- isTemplate doc = not $ isInfoboxTemplate config tag
      | otherwise                  = False

-- | For testing.
parseSkeleton :: String -> Either String [PageSkeleton]
parseSkeleton =
    fmap (docsToSkeletons defaultConfig (SiteId "Test Site") (PageId "Test Page")) . Markup.parse

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

toSkeleton :: Config -> SiteId -> PageId -> [Doc] -> [PageSkeleton]
toSkeleton config siteId thisPage = go
  where
    go [] = []
    go docs
      | Just (para, rest) <- splitParagraph siteId thisPage docs
      = Para para : go rest
    go (doc : docs)
      | Just (target, caption) <- isImage doc
      = Image target (go caption) : go docs
      | Markup.List tys content <- doc
      = let contents = concat $ mapMaybe (toParaBody siteId thisPage) content
        in CAR.Types.List (length tys) (mkParagraph contents) : go docs
    go (Heading lvl title : docs) =
        let (children, docs') = break isParentHeader docs
            isParentHeader (Heading lvl' _) = lvl' <= lvl
            isParentHeader _                = False
            heading = SectionHeading $ resolveEntities $ T.pack $ getAllText title
        in Section heading (sectionHeadingToId heading) (go children) : go docs'
    go (Template [Text tag] args : docs)
      | isInfoboxTemplate config tag' =
        Infobox tag' (mapMaybe f args) : go docs
      where
        tag' = T.toCaseFold $ T.pack tag
        f (Just key, val) = Just (key, go val)
        f _               = Nothing
    go (_ : docs) = go docs
