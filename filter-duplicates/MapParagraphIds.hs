{-# LANGUAGE RecordWildCards #-}

import Data.Maybe
import Data.Monoid
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.ByteString.Lazy as BSL
import Options.Applicative

import CAR.CarExports
import CAR.Types
import CAR.FilterDuplicates.ConnectedComponent
import CAR.FilterDuplicates.Utils
import CAR.RunFile
import CAR.QRelFile



main :: IO ()
main = do
    mode <- execParser $ info (helper <*> modes) (progDesc "Converting paragraph ids from run files and qrels files according to duplicates mapping.\n\n")
    mode

modes = subparser $ command "run" ( info (helper <*> runMode) mempty)
--                     <> command "entitypassage-run" $ info (run <$> argument str (help "run file with entity passage entries"))
                   <> command "qrel"  ( info (helper <*> qrelMode) mempty)

duplicatesArg :: Parser FilePath
duplicatesArg = option str (short 'd' <> long "duplicates" <> help "duplicates file")

outputArg :: Parser FilePath
outputArg = option str (short 'o' <> long "output" <> help "Output file" )

runMode :: Parser (IO ())
runMode =
    run <$> argument str (help "run file over paragraph ids")
        <*> duplicatesArg
        <*> outputArg
    where
      run :: FilePath -> FilePath -> FilePath -> IO ()
      run runFile duplicatesFile outputFile = do
          entries <- readParagraphRun runFile
          -- paragraphMapper :: ParagraphId -> ParagraphId
          paragraphMapper <- mapToNewParagraphIds . HM.fromList . parseDuplicates <$> readFile duplicatesFile

          let entries' = fmap mapDocument entries
                where mapDocument entry = entry { carDocument = paragraphMapper (carDocument entry) }
          writeParagraphRun outputFile entries'

qrelMode :: Parser (IO ())
qrelMode =
    run <$> argument str (help "qrel file over paragraph ids")
        <*> duplicatesArg
        <*> outputArg
    where
      run :: FilePath -> FilePath -> FilePath -> IO ()
      run runFile duplicatesFile outputFile = do
          entries <- readParagraphQRel runFile
          -- paragraphMapper :: ParagraphId -> ParagraphId
          paragraphMapper <- mapToNewParagraphIds . HM.fromList . parseDuplicates <$> readFile duplicatesFile

          let entries' = fmap mapDocument entries
                where mapDocument (Annotation sp paraId rel) =
                          Annotation sp (paragraphMapper paraId) rel
          writeParagraphQRel outputFile entries'






mapToNewParagraphIds :: HM.HashMap ParagraphId ParagraphId -> ParagraphId -> ParagraphId
mapToNewParagraphIds deduplicationTable paraId =
    fromMaybe paraId $ paraId `HM.lookup` deduplicationTable


