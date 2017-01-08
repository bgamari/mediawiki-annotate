import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import System.IO
import qualified Data.HashSet as HS
import Options.Applicative
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as BSB
import Data.Hashable
import CAR.Types

opts :: Parser (FilePath, FilePath, Bool -> Bool, IO (Page -> Bool))
opts =
    (,,,)
    <$> argument str (help "input file" <> metavar "ANNOTATIONS FILE")
    <*> option str (short 'o' <> long "output")
    <*> flag id not (long "not" <> help "negate match")
    <*> predicate
  where
    predicate :: Parser (IO (Page -> Bool))
    predicate =
        testPred <|> trainPred <|> namePred <|> categoryPred

    testPred = flag' f (long "test")
      where f = pure $ even . hash . pageName

    trainPred = flag' f (long "train")
      where f = pure $ odd . hash . pageName

    namePred = option (f <$> str) (long "name-set" <> metavar "FILE"
                                   <> help "file containing names to be matched")
      where f fname = do
                pages <- HS.fromList . map (PageName . T.pack) . lines <$> readFile fname
                return $ \page -> (pageName page) `HS.member` pages

    categoryPred =
        f <$> option str (long "category-substring" <> metavar "STRING"
                          <> help "category substring to match")
      where f fname = do
                patterns <-  map (T.toCaseFold . T.pack) . lines <$> readFile fname
                let matches cat = any (\p ->  p `T.isInfixOf` cat) patterns
                return $ \page -> any (\cat -> matches $ T.toCaseFold cat) (pageCategories page)

pageCategories :: Page -> [T.Text]
pageCategories page =
    mapMaybe isCategoryTag $ foldMap skeletonLinks $ pageSkeleton page
  where
    skeletonLinks :: PageSkeleton -> [PageName]
    skeletonLinks (Section _ _ children) = foldMap skeletonLinks children
    skeletonLinks (Para (Paragraph _ bodies)) = foldMap paraBodyLinks bodies

    paraBodyLinks :: ParaBody -> [PageName]
    paraBodyLinks (ParaLink pageName _ ) = [pageName]
    paraBodyLinks (ParaText _ ) = []

    isCategoryTag :: PageName -> Maybe T.Text
    isCategoryTag (PageName pageName) =
        T.pack "Category:" `T.stripPrefix` pageName


main :: IO ()
main = do
    (inputFile, outputFile, polarity, getPredicate) <- execParser $ info (helper <*> opts) mempty
    predicate <- (polarity .) <$> getPredicate
    pages <- decodeCborList <$> BSL.readFile inputFile
    withFile outputFile WriteMode $ \h ->
        BSB.hPutBuilder h $ encodeCborList $ filter predicate pages
