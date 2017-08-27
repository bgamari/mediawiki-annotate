{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}

import Data.Semigroup hiding (All, Any, option)
import Options.Applicative

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.IO as T

import CAR.Types

import SimplIR.Format.QRel
import GraphExpansionExperiments


opts :: Parser (FilePath, FilePath, FilePath)
opts =
    (,,)
    <$> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")
    <*> option str (long "qrels" <> metavar "QRELS" <> help "qrel file to filter")
    <*> option str (short 'q' <> long "queries" <> metavar "CBOR" <> help "Queries from CBOR pages")


main :: IO ()
main = do
    (outputFile, qrelfile, queryFile) <-
        execParser $ info (helper <*> opts) mempty

    let toSeeds :: QueryDoc -> HS.HashSet PageId
        toSeeds queryDoc =
             queryDocPageId queryDoc `HS.insert` queryDocLeadEntities queryDoc

        pagesToForbiddenEntities :: [Page] -> [(PageId, HS.HashSet PageId)]
        pagesToForbiddenEntities pages = [ (queryDocPageId queryDoc, toSeeds queryDoc)
                                         | queryDoc <- pagesToQueryDocs id QueryFromPageTitle pages
                                         ]


    query2ForbiddenEntities <- HM.fromList . pagesToForbiddenEntities
                            <$> readPagesFile queryFile
        :: IO (HM.HashMap PageId (HS.HashSet PageId))

    let notEntryWithSeed :: Entry IsRelevant -> Bool
        notEntryWithSeed Entry {queryId = queryId, documentName = entityId } =
            case (packPageId $ T.unpack queryId) `HM.lookup` query2ForbiddenEntities of
              Just seeds -> not $ (packPageId $ T.unpack entityId) `HS.member` seeds
              _          -> True
    qrelEntries <- filter notEntryWithSeed <$> readQRel binaryRelevance qrelfile
    let formatQrels :: Entry IsRelevant -> T.Text
        formatQrels Entry {..} =
          T.unwords [ queryId
                  , "0"
                  , documentName
                  , case relevance of
                      Relevant    -> "1"
                      NotRelevant -> "0"
                  ]

    T.writeFile outputFile
        $ T.unlines
        $ fmap formatQrels
        qrelEntries
