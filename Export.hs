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

    putStr "Writing stub skeletons..."
    let skeletonFile = path <.> "skeletons"
    withFile skeletonFile WriteMode $ \h ->
        BSB.hPutBuilder h $ encodeCborList $ map toStubSkeleton (pages anns)
    putStrLn "done"

    putStr "Writing paragraphs..."
    let paragraphsFile = path <.> "paragraphs"
    withFile paragraphsFile WriteMode $ \h ->
        BSB.hPutBuilder h $ encodeCborList $ concatMap toParagraphs (pages anns)
    putStrLn "done"

    putStr "Writing relevance annotations..."
    let relsFile = path <.> "qrel"
    withFile relsFile WriteMode $ \h ->
          hPutStrLn h $ unlines $ map prettyAnnotation $ concatMap Exports.toAnnotations $ (pages anns)
    putStrLn "done"

    return ()
