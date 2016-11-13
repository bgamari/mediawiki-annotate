import qualified Data.ByteString.Builder as BSB
import System.IO
import System.FilePath

import Options.Applicative

import CAR.AnnotationsFile
import CAR.Types
import CAR.CarExports

options :: Parser (FilePath)
options =
    argument str (help "annotations file")

main :: IO ()
main = do
    (path) <- execParser $ info (helper <*> options) mempty
    anns <- openAnnotations path

    putStrLn "Writing skeletons..."
    let skeletonFile = path <.> "skeletons"
    withFile skeletonFile WriteMode $ \h ->
        BSB.hPutBuilder h $ encodeCborList $ map toStubSkeleton (pages anns)
    putStrLn "done"

    return ()
