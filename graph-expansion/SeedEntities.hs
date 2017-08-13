import Data.Semigroup hiding (All, Any, option)
import Options.Applicative

import qualified Data.HashSet as HS
import qualified Data.ByteString.Lazy as BSL

import CAR.Types
import GraphExpansionExperiments

opts :: Parser (FilePath)
opts =
    id
    <$> argument str (help "queryFile file" <> metavar "ANNOTATIONS FILE")

main :: IO ()
main = do
    (queryFile) <-
        execParser $ info (helper <*> opts) mempty

    queriesToSeedEntities <- pagesToQueryDocs id QueryFromPageTitle . decodeCborList <$> BSL.readFile queryFile
    putStrLn $ unlines $ foldMap toSeedLines $ queriesToSeedEntities
  where
    toSeedLines queryDoc =
           [ (unpackPageId $ queryDocQueryId queryDoc)++" 0 "++ (unpackPageId seed) ++ " 1"
           | seed <- HS.toList $ queryDocLeadEntities queryDoc]
