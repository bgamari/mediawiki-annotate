import Options.Applicative

import qualified Data.HashSet as HS

import CAR.Types
import GraphExpansionExperiments
import qualified CAR.AnnotationsFile as CAR

opts :: Parser (FilePath)
opts = argument str (help "queryFile file" <> metavar "PAGES FILE")

main :: IO ()
main = do
    (queryFile) <-
        execParser $ info (helper <*> opts) mempty
    pageBundle <- CAR.openPageBundle queryFile
--     siteId <- wikiSite . fst <$> readPagesFileWithProvenance queryFile
    let queriesToSeedEntities = pagesToQueryDocs pageBundle QueryFromPageTitle
    putStrLn $ unlines $ foldMap toSeedLines $ queriesToSeedEntities
  where
    toSeedLines queryDoc =
           [ (unpackPageId $ queryDocPageId queryDoc)++" 0 "++ (unpackPageId seed) ++ " 1"
           | seed <- HS.toList $ queryDocLeadEntities queryDoc]
