import Data.Semigroup hiding (All, Any, option)
import Options.Applicative

import qualified Data.HashSet as HS

import CAR.Types
import GraphExpansionExperiments

opts :: Parser (FilePath)
opts = argument str (help "queryFile file" <> metavar "PAGES FILE")

main :: IO ()
main = do
    (queryFile) <-
        execParser $ info (helper <*> opts) mempty

    siteId <- wikiSite . fst <$> readPagesFileWithProvenance queryFile
    queriesToSeedEntities <- pagesToQueryDocs siteId QueryFromPageTitle <$> readPagesFile queryFile
    putStrLn $ unlines $ foldMap toSeedLines $ queriesToSeedEntities
  where
    toSeedLines queryDoc =
           [ (unpackPageId $ queryDocPageId queryDoc)++" 0 "++ (unpackPageId seed) ++ " 1"
           | seed <- HS.toList $ queryDocLeadEntities queryDoc]
