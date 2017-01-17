{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid hiding (All, Any)
import Data.Void
import Control.Monad (void)
import System.IO
import Options.Applicative
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as BSB
import qualified Text.Trifecta as Tri
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Text.PrettyPrint.ANSI.Leijen ((<$$>))

import CAR.Types
import FilterPred

helpDescr :: PP.Doc
helpDescr =
    "Predicate options:" <$$> PP.indent 4 opts
  where
    cmd a b = PP.nest 8 (a <$$> b)
    opts = PP.vsep
      [ cmd "train-set"                        "matches pages in the training set",
        cmd "test-set"                         "matches pages in the test set",
        cmd "fold K"                           "matches pages in fold k (k in 0..4)",
        cmd "is-redirect"                      "matches redirect pages",
        cmd "is-disambiguation"                "matches disambiguation pages",
        cmd "page-hash-mod N K"                "matches pages where the page id mod N == K, for N > K ",
        "",
        cmd "name-contains SUBSTR"             "matches pages where the page name contains the SUBSTR (case insensitive)",
        cmd "name-has-prefix PREFIX"           "matches pages where the page name starts with PREFIX (case sensitive)",
        cmd "name-in-set [NAME1, NAME2, ...]"  "matches pages where the page name is exactly NAME1 or NAME2, ... (case sensitive with the exception of the first letter)",
        cmd "category-contain SUBSTR"          "matches pages that are a member of a category that contains SUBSTR (case insensitive)",
        "",
        cmd "category-contains-from-file FILE" "like category-contain but loads SUBSTRs from FILE",
        cmd "name-set-from-file FILE"          "like name-in-set but loads NAMEs from FILE",
        "",
        cmd "true"                             "always true",
        cmd "PRED1 | PRED2"                    "Boolean OR, matches predicate PRED1 or PRED2",
        cmd "PRED1 & PRED2"                    "Boolean AND, matches predicate PRED1 and PRED2",
        cmd "! PRED"                           "Boolean NOT, inverts the predicate PRED"
      ]

opts :: Parser (FilePath, FilePath, Maybe Int, Pred PredFromFile)
opts =
    (,,,)
    <$> argument str (help "input file" <> metavar "ANNOTATIONS FILE")
    <*> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")
    <*> option (Just <$> auto) (short 'n' <> long "take" <> metavar "N" <> help "Take the first N pages")
    <*> argument predicate (metavar "PRED" <> help "Predicate")
  where
    predicate = do
        s <- str
        case Tri.parseString (FilterPred.pred predFromFile <* Tri.eof) mempty s of
          Tri.Success p -> return p
          Tri.Failure e -> fail $ show $ Tri._errDoc e

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
    (inputFile, outputFile, takeN, predicate) <- execParser $ info (helper <*> opts) (progDescDoc $ Just helpDescr)
    pages <- decodeCborList <$> BSL.readFile inputFile
    predicate' <- runPredFromFile predicate
    withFile outputFile WriteMode $ \h ->
        BSB.hPutBuilder h $ encodeCborList
            $ maybe id take takeN
            $ filter (interpret predicate') pages
