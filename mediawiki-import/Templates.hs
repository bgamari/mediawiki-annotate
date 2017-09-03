{-# LANGUAGE OverloadedStrings #-}
module Templates where

import Data.Semigroup
import Data.Maybe
import Data.Char
import Data.List

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.HashMap.Strict as HM

import Utils
import Data.MediaWiki.Markup

type TemplateHandler = [(Maybe Text, [Doc])] -> Maybe [Doc]

runTemplateHandler :: (TemplateTag -> TemplateHandler) -> Doc -> [Doc]
runTemplateHandler handler (Template tmpl' args)
  | Just res <- handler tmpl args = concatMap (runTemplateHandler handler) res
  where tmpl = T.toCaseFold $ T.pack $ getAllText tmpl'
runTemplateHandler _ x = [x]

defaultTemplateHandler :: TemplateTag -> TemplateHandler
defaultTemplateHandler tag args
  | "IPA-" `T.isPrefixOf` tag    = Just []
  | "IPAc-" `T.isPrefixOf` tag   = Just []
  | "lang-" `T.isPrefixOf` tag
  , ((Nothing, body):_) <- args  = Just body
  | "Infobox" `T.isPrefixOf` tag = Just [] -- don't show infoboxes, even with alt

  | Just alt <- lookupNamed "alt" args = Just alt
  | Just handler <- HM.lookup tag templates
  = handler args
defaultTemplateHandler _ _ = Nothing

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

    -- \ discards
    dropTemplate _ = Nothing

    listTemplate args =
        Just $ map (BulletList 1) (mapMaybe isUnnamed args)

    convertTemplate ((Nothing, val) : (Nothing, unit) : _) =
        justText $ getAllText val <> " " <> getAllText unit
    convertTemplate _ = Nothing

    -- \ preserves the second argument
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

    -- \ preserves the first argument
    simpleTemplate ((Nothing, val) : _) = Just val
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
