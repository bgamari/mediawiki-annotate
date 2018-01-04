{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Control.Exception
import Data.Tuple
import Data.Semigroup hiding (All, Any, option)
import Options.Applicative
import System.IO
import Data.Aeson
import Numeric.Log

import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL

import CAR.Types
import CAR.ToolVersion
import CAR.AnnotationsFile as AnnsFile
import CAR.Retrieve as Retrieve
import qualified CAR.RunFile as CarRun
import CAR.Utils (nubWithKey)

import WriteRanking
import GraphExpansionExperiments
import qualified SimplIR.SimpleIndex as Index
import qualified SimplIR.SimpleIndex.Models.BM25 as BM25
import qualified SimplIR.SimpleIndex.Models.QueryLikelihood as QL
import SimplIR.TopK (collectTopK)

type NumResults = Int

type EntityIndex = Index.OnDiskIndex Term PageId Int

data SeedDerivation = SeedsFromLeadSection
                    | SeedsFromEntityIndex EntityIndex
                    -- | SeedsFromHeadingEntityLinks -- TODO

data QuerySource = QueriesFromCbor FilePath QueryDerivation SeedDerivation
                 | QueriesFromJson FilePath

data Graphset = Fullgraph | Subgraph
data RankingType = EntityRanking | EntityPassageRanking
  deriving (Show)

opts :: Parser ( FilePath
               , FilePath
               , QuerySource
               , Index.OnDiskIndex Term ParagraphId Int
               , [CarRun.QueryId]
               , NumResults
               )
opts =
    (,,,,,)
    <$> argument str (help "articles file" <> metavar "ANNOTATIONS FILE")
    <*> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")
    <*> querySource
    <*> option (Index.OnDiskIndex <$> str)
               (short 'i' <> long "index" <> metavar "INDEX" <> help "simplir paragraph index")
    <*> many (option (CarRun.QueryId . T.pack <$> str) (long "query" <> metavar "QUERY" <> help "execute only this query"))
    <*> option auto (short 'k' <> long "num-results" <> help "number of results per query")
    where

      querySource :: Parser QuerySource
      querySource =
              fromCborTitle
          <|> option (fmap QueriesFromJson str) (short 'j' <> long "queries-json" <> metavar "JSON" <> help "Queries from JSON")
          <|> fromEntityIndex
        where
          queryDeriv =
              flag QueryFromPageTitle QueryFromSectionPaths
                   (long "query-from-sections" <> help "Use sections as query documents")
          fromCborTitle =
              QueriesFromCbor
                <$> option str (short 'q' <> long "queries" <> metavar "CBOR" <> help "Queries from CBOR pages")
                <*> queryDeriv
                <*> pure SeedsFromLeadSection

          fromEntityIndex =
              QueriesFromCbor
                <$> option str (short 'Q' <> long "queries-nolead" <> metavar "CBOR" <> help "Queries from CBOR pages taking seed entities from entity retrieval")
                <*> queryDeriv
                <*> option (SeedsFromEntityIndex . Index.OnDiskIndex <$> str) (long "entity-index" <> metavar "INDEX" <> help "Entity index path")

retrievalModels' :: [(RetrievalFun, Index.RetrievalModel Term ParagraphId Int)]
retrievalModels' =
    [ (Ql,   QL.queryLikelihood $ QL.Dirichlet 100)
    , (Bm25, BM25.bm25 $ BM25.sensibleParams)
    ]

computeRankingsForQuery :: (Index.RetrievalModel Term ParagraphId Int -> RetrievalFunction ParagraphId)
                        -> AnnotationsFile
                        -> CarRun.QueryId  ->  [Term]
                        -> [(RetrievalFun, [(ParagraphId, Double)])]

computeRankingsForQuery
      retrieveDocs
      annsFile queryId query =
      let

        retrievalResults :: [(RetrievalFun, [(ParagraphId, Double)])]
        retrievalResults = [ (irname, retrievalResult)
                           | (irname, retrievalModel) <- retrievalModels'
                           , let retrievalResult =
                                   fmap (\(ed, ld) -> (ed, ln ld)) $
                                   retrieveDocs retrievalModel query
                           ]


    in retrievalResults


logMsg :: CarRun.QueryId -> RetrievalFun -> String -> IO ()
logMsg queryId method t = T.putStr $ (CarRun.unQueryId queryId)<>"\t"<>T.pack (show method)<>"\t"<>T.pack t<>"\n"




main :: IO ()
main = do
    hSetBuffering stdout LineBuffering

    (articlesFile, outputFilePrefix, querySrc, simplirIndexFilepath,
      queryRestriction, numResults) <-
        execParser' 1 (helper <*> opts) mempty
    putStrLn $ "# Pages: " ++ show articlesFile
    annsFile <- AnnsFile.openAnnotations articlesFile
    siteId <- wikiSite . fst <$> readPagesFileWithProvenance articlesFile
    putStrLn $ "# Query restriction: " ++ show queryRestriction
    putStrLn $ "# Paragraph index: "++ show simplirIndexFilepath

    queries' <-
        case querySrc of
          QueriesFromCbor queryFile queryDeriv _seedDeriv -> do
              pagesToQueryDocs siteId queryDeriv <$> readPagesOrOutlinesAsPages queryFile

          QueriesFromJson queryFile -> do
              QueryDocList queries <- either error id . Data.Aeson.eitherDecode <$> BSL.readFile queryFile
              return queries


    queries <-
        if null queryRestriction
          then return queries'
          else do putStrLn $ "# using only queries "<>show queryRestriction
                  return $ filter (\q-> queryDocQueryId q `elem` queryRestriction) queries'
    putStrLn $ "# query count: " ++ show (length queries)


    index <- Index.open simplirIndexFilepath
    let retrieveDocs :: Index.RetrievalModel Term ParagraphId Int -> RetrievalFunction ParagraphId
        retrieveDocs model = map swap . collectTopK numResults  . Index.score index model
    putStrLn "# opened paragraph index"

    let allIrMethods = [minBound :: RetrievalFun .. maxBound]


    runHandles <- sequence $ M.fromList  -- todo if we run different models in parallel, this will overwrite previous results.
      [ (method, openFile (outputFilePrefix ++ (show method) ++ ".run") WriteMode >>= newMVar)
      | method <- allIrMethods ]
        :: IO (M.Map RetrievalFun (MVar Handle))

    ncaps <- getNumCapabilities
    let --forM_' = forM_
        forM_' = forConcurrentlyN_ ncaps

        runIrMethod :: CarRun.QueryId -> QueryDoc -> RetrievalFun -> [(ParagraphId, Double)] -> IO ()
        runIrMethod queryId query method ranking' = handle onError $ do
            let Just hdl = M.lookup method runHandles
                ranking = nubWithKey fst ranking'


            logMsg queryId method $ "evaluating"
            --logTimed "evaluating graph" $ evaluate $ rnf graph
            --logMsg $ "graph size: "++show (graphSize graph)
            --ranking <- logTimed "computing ranking" $ evaluate $ force $ computeRanking graph
            logMsg queryId method $ "ranking entries="++show (length ranking)

            let formatted = WriteRanking.formatPassageRankings
                             (T.pack $ show method)
                             (CarRun.unQueryId queryId)
                             $ ranking
            bracket (takeMVar hdl) (putMVar hdl) $ \ h -> do
                TL.hPutStrLn h formatted
                hFlush h
          where
            onError (SomeException exc) =
                putStrLn $ concat [ "error: exception while running "
                                  , show method, " on ", show queryId, ": ", show exc ]

    let subgraphExpansion =
            forM_' queries $ -- was: queriesWithSeedEntities
              \query@QueryDoc{queryDocQueryId=queryId, queryDocPageId=queryPage, queryDocLeadEntities=seedEntities} -> do



                T.putStr $ T.pack $ "# Processing query "++ show query++": seeds=" ++ show seedEntities ++ "\n"
                let rankings :: [(RetrievalFun, [(ParagraphId,  Double)])]
                    rankings = computeRankingsForQuery retrieveDocs annsFile queryId (queryDocRawTerms query)


                mapM_ (\(method, ranking) -> runIrMethod queryId query method ranking) rankings

    subgraphExpansion
    mapM_ (\h -> takeMVar h >>= hClose) runHandles

-- | Given a foldable container of a's, and function that turns an a into an action, run all actions throw away result.
-- Run N actions concurrentl at a time.
forConcurrentlyN_ :: Foldable f => Int -> f a -> (a -> IO ()) -> IO ()
forConcurrentlyN_ n xs f = do
    sem <- atomically $ newTSem n
    let run x = bracket (atomically $ waitTSem sem) (const $ atomically $ signalTSem sem) (const $ f x)
    forConcurrently_ xs run
