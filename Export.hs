{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe
import qualified Data.ByteString.Builder as BSB
import System.IO
import System.FilePath

import Options.Applicative

import CAR.AnnotationsFile
import CAR.Types
import CAR.CarExports as Exports

options :: Parser (FilePath)
options =
    argument str (help "annotations file")

main :: IO ()
main = do
    (path) <- execParser $ info (helper <*> options) mempty
    anns <- openAnnotations path
    let names = [ "Green sea turtle"
                , "Hydraulic fracturing"
                , "Spent nuclear fuel"
                , "Food vs. fuel"
                , "Sustainable biofuel"
                , "Behavior of nuclear fueld during a reactor accident"
                ]
    let pages = mapMaybe (`lookupPage` anns) names
        {-# INLINE pages #-}

    putStr "Writing stub skeletons..."
    let skeletonFile = path <.> "skeletons"
    withFile skeletonFile WriteMode $ \h ->
        BSB.hPutBuilder h $ encodeCborList $ map toStubSkeleton pages
    putStrLn "done"

    putStr "Writing paragraphs..."
    let paragraphsFile = path <.> "paragraphs"
    withFile paragraphsFile WriteMode $ \h ->
        BSB.hPutBuilder h $ encodeCborList $ concatMap toParagraphs pages
    putStrLn "done"

    putStr "Writing relevance annotations..."
    let relsFile = path <.> "qrel"
    withFile relsFile WriteMode $ \h ->
          hPutStr h $ unlines $ map prettyAnnotation $ concatMap Exports.toAnnotations pages
    putStrLn "done"

    return ()
