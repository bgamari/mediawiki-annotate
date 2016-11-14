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

    putStrLn "Writing stubs skeletons..."
    let skeletonFile = path <.> "skeletons"
    withFile skeletonFile WriteMode $ \h ->
        BSB.hPutBuilder h $ encodeCborList $ map toStubSkeleton (pages anns)
    putStrLn "done"

    putStrLn "Writing paragraphs..."
    let paragraphsFile = path <.> "paragraphs"
    withFile paragraphsFile WriteMode $ \h ->
        BSB.hPutBuilder h $ encodeCborList $ map toParagraphs (pages anns)
    putStrLn "done"

    putStrLn "Writing relevance annotations..."
    let relsFile = path <.> "qrel"
    withFile relsFile WriteMode $ \h ->
          hPutStrLn h $ unlines $ map prettyAnnotation $ concatMap Exports.toAnnotations $ (pages anns)
    putStrLn "done"

    return ()
