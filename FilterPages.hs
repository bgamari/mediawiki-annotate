{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid hiding (All, Any)
import Data.Void
import System.IO
import Options.Applicative
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as BSB
import qualified Text.Trifecta as Tri

import CAR.Types
import FilterPred

opts :: Parser (FilePath, FilePath, Pred PredFromFile)
opts =
    (,,)
    <$> argument str (help "input file" <> metavar "ANNOTATIONS FILE")
    <*> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")
    <*> argument predicate (metavar "PRED" <> help "Predicate")
  where
    predicate = do
        s <- str
        case Tri.parseString (FilterPred.pred predFromFile <* Tri.eof) mempty s of
          Tri.Success p -> return p
          Tri.Failure e -> fail $ show e

data PredFromFile = NameSetFromFile FilePath
                  | HasCategoryContainingFromFile FilePath
                  deriving (Show)

predFromFile :: Tri.Parser PredFromFile
predFromFile =
    nameSet <|> hasCategoryContaining
  where
    nameSet = do
        Tri.textSymbol "name-set-from-file"
        NameSetFromFile <$> Tri.stringLiteral

    hasCategoryContaining = do
        Tri.textSymbol "category-contains-from-file"
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
    (inputFile, outputFile, predicate) <- execParser $ info (helper <*> opts) mempty
    pages <- decodeCborList <$> BSL.readFile inputFile
    predicate' <- runPredFromFile predicate
    withFile outputFile WriteMode $ \h ->
        BSB.hPutBuilder h $ encodeCborList $ filter (interpret predicate') pages
