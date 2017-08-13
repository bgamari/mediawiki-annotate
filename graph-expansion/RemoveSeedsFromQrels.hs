{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}

import Data.Semigroup hiding (All, Any, option)
import Options.Applicative

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.IO as T

import CAR.Types
--import CAR.AnnotationsFile as AnnsFile

import SimplIR.Format.QRel
import GraphExpansionExperiments


opts :: Parser ( FilePath, FilePath, FilePath, FilePath)
opts =
    (,,,)
    <$> argument str (help "articles file" <> metavar "ANNOTATIONS FILE")
    <*> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")
    <*> option str (long "qrels" <> metavar "QRELS" <> help "qrel file to filter")
    <*> option str (short 'q' <> long "queries" <> metavar "CBOR" <> help "Queries from CBOR pages")



main :: IO ()
main = do
    (articlesFile, outputFile, qrelfile, queryFile) <-
        execParser $ info (helper <*> opts) mempty

--     annsFile <- AnnsFile.openAnnotations articlesFile

    let toSeeds :: QueryDoc -> HS.HashSet PageId
        toSeeds queryDoc =
             queryDocPageId queryDoc `HS.insert` queryDocLeadEntities queryDoc

        pagesToForbiddenEntities :: [Page] -> [(PageId, HS.HashSet PageId)]
        pagesToForbiddenEntities pages = [ ( queryDocPageId queryDoc, toSeeds queryDoc)
                                         | queryDoc <- pagesToQueryDocs id QueryFromPageTitle pages
                                         ]


    query2ForbiddenEntities <- HM.fromList . pagesToForbiddenEntities
                            <$> readCborList queryFile
        :: IO (HM.HashMap PageId (HS.HashSet PageId))

    let notEntryWithSeed :: Entry -> Bool
        notEntryWithSeed Entry {queryId = queryId, documentName = entityId } =
            case (packPageId $ T.unpack queryId) `HM.lookup` query2ForbiddenEntities of
              Just seeds -> not $ (packPageId $ T.unpack entityId) `HS.member` seeds
              _          -> True
    qrelEntries <- filter notEntryWithSeed <$> readQRel qrelfile
    let formatQrels :: Entry -> T.Text
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
