{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe
import Data.Monoid
import System.IO

import qualified Data.Text as T
import qualified Data.ByteString.Builder as BSB
import System.FilePath

import Options.Applicative

import CAR.AnnotationsFile
import CAR.Types
import CAR.CarExports as Exports

options :: Parser (FilePath, [PageName])
options =
    (,) <$> argument str (help "annotations file")
        <*> many (option (PageName . T.pack <$> str)
                         (short 'p' <> long "page" <> help "Export only this page"))

main :: IO ()
main = do
    (path, names) <- execParser $ info (helper <*> options) mempty
    anns <- openAnnotations path
    let pagesToExport
          | null names = pages anns
          | otherwise  = mapMaybe (`lookupPage` anns) names
        {-# INLINE pagesToExport #-}

    putStr "Writing stub skeletons..."
    let skeletonFile = path <.> "skeletons"
    withFile skeletonFile WriteMode $ \h ->
        BSB.hPutBuilder h $ encodeCborList $ map toStubSkeleton pagesToExport
    putStrLn "done"

    putStr "Writing paragraphs..."
    let paragraphsFile = path <.> "paragraphs"
    withFile paragraphsFile WriteMode $ \h ->
        BSB.hPutBuilder h $ encodeCborList $ concatMap toParagraphs pagesToExport
    putStrLn "done"

    putStr "Writing relevance annotations..."
    let relsFile = path <.> "qrel"
    withFile relsFile WriteMode $ \h ->
          hPutStr h $ unlines $ map prettyAnnotation $ concatMap Exports.toAnnotations pagesToExport
    putStrLn "done"

    return ()
