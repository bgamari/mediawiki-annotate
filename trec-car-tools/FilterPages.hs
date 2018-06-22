{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid hiding (All, Any)
import Data.Void
import Control.Monad (void)
import Options.Applicative
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Text.Trifecta as Tri
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import qualified Data.Binary.Serialise.CBOR as CBOR
import Text.PrettyPrint.ANSI.Leijen ((<$$>))

import CAR.ToolVersion
import CAR.Types
import FilterPred
import WikiData

helpDescr :: PP.Doc
helpDescr =
    "Predicate syntax:" <$$> PP.indent 4 predicateHelp <$$> "" <$$> " Example: trec-car-filter in.cbor -o out.cbor '( ! is-redirect )'   "
  where
    cmd a b = PP.nest 8 (a <$$> b)
    predicateHelp = PP.vsep
      [ cmd "train-set"                        "matches pages in the training set",
        cmd "test-set"                         "matches pages in the test set",
        cmd "fold K"                           "matches pages in fold k (k in 0..4)",
        cmd "is-redirect"                      "matches redirect pages",
        cmd "is-disambiguation"                "matches disambiguation pages",
        cmd "is-category"                      "matches category pages",
        cmd "page-hash-mod N K [SALT]"         "matches pages where (hash of the page name) mod N == K, for N > K ",
        "",
        cmd "name-contains SUBSTR"             "matches pages where the page name contains the SUBSTR (case insensitive)",
        cmd "name-has-prefix PREFIX"           "matches pages where the page name starts with PREFIX (case sensitive)",
        cmd "category-contain SUBSTR"          "matches pages that are a member of a category that contains SUBSTR (case insensitive)",
        "",
        cmd "name-in-set [\"P1\", \"P2\", \"P3\"]"  "matches pages whose page names are in the given set {P1,P2,P3}",
        cmd "name-set-from-file FILE"          "like name-in-set but loads NAMEs from FILE",
        cmd "category-contains-from-file FILE" "like category-contain but loads SUBSTRs from FILE",
        "",
        cmd "true"                             "always true",
        cmd "PRED1 | PRED2"                    "Boolean OR, matches predicate PRED1 or PRED2",
        cmd "PRED1 & PRED2"                    "Boolean AND, matches predicate PRED1 and PRED2",
        cmd "! PRED"                           "Boolean NOT, inverts the predicate PRED"
      ]

opts :: Parser (FilePath, FilePath, Maybe Int, Maybe (FilePath, SiteId, SiteId), Pred PredFromFile)
opts =
    (,,,,)
    <$> argument str (help "input file" <> metavar "ANNOTATIONS FILE")
    <*> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")
    <*> optional (option auto  (short 'n' <> long "take" <> metavar "N" <> help "Take the first N pages"))
    <*> optional multiLangOpts
    <*> argument predicate (metavar "PRED" <> help "Predicate")
  where
    predicate = do
        s <- str
        case Tri.parseString (FilterPred.parsePred predFromFile <* Tri.eof) mempty s of
          Tri.Success p -> return p
          Tri.Failure e -> fail $ show $ Tri._errDoc e
    multiLangOpts =
        (,,)
          <$> option str (short 'L' <> long "lang-index" <> metavar "LANGIDX" <> help "Inter-site page name mapping")
          <*> option siteId (long "from-site" <> metavar "FROMSITE" <> help "language of this archive")
          <*> option siteId (long "to-site" <> metavar "TOSITE" <> help "canonical site (enwiki)" <> value (SiteId "enwiki"))
    siteId = SiteId . T.pack <$> str

data PredFromFile = NameSetFromFile FilePath
                  | HasCategoryContainingFromFile FilePath
                  deriving (Show)

predFromFile :: Tri.Parser PredFromFile
predFromFile =
    nameSet <|> hasCategoryContaining
  where
    nameSet = do
        void $ Tri.textSymbol "name-set-from-file"
        NameSetFromFile <$> Tri.stringLiteral

    hasCategoryContaining = do
        void $ Tri.textSymbol "category-contains-from-file"
        HasCategoryContainingFromFile <$> Tri.stringLiteral

runPredFromFile :: Pred PredFromFile -> IO (Pred Void)
runPredFromFile = runPred go
  where
    go (NameSetFromFile path) =
        NameInSet . HS.fromList . map (PageName . T.pack) . lines <$> readFile path
    go (HasCategoryContainingFromFile path) =
        Any . map (HasCategoryContaining . T.pack) . filter (not . null) . lines <$> readFile path

main :: IO ()
main = do
    (inputFile, outputFile, takeN, multiLangOptsMaybe, predicate) <-
        execParser' 1 (helper <*> opts) (progDescDoc $ Just helpDescr)
    (prov, pages) <- readPagesFileWithProvenance inputFile
    predicate' <- runPredFromFile predicate

    pageNameTranslate <- case multiLangOptsMaybe of
                           Nothing -> return $ id
                           Just (siteWikiIndex, fromSite, toSite)  -> do
                               siteIndex <- CBOR.readFileDeserialise siteWikiIndex
                               let siteLookup = createLookup siteIndex fromSite toSite
                               let pageNameTranslate :: PageName -> PageName
                                   pageNameTranslate =
                                       \fromPageName -> case HM.lookup fromPageName siteLookup of
                                                          Just toPageName -> toPageName
                                                          Nothing -> fromPageName
                               return $ pageNameTranslate

    writeCarFile outputFile prov
        $ maybe id take takeN
        $ filter (interpret pageNameTranslate predicate') pages
