{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad (when, void)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Control.Exception
import Data.List (sortBy)
import Data.Maybe
import Data.Tuple
import Data.Semigroup hiding (All, Any, option)
import Data.Foldable
import Data.Coerce
import Options.Applicative
import System.IO
import Data.Time.Clock
import Numeric
import GHC.TypeLits
import Data.Aeson
import Numeric.Log

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

import qualified Data.GraphViz as Dot
import qualified Data.GraphViz.Printing as Dot
import qualified Data.GraphViz.Attributes.Complete as Dot
import qualified Data.GraphViz.Commands.IO as Dot
import CAR.Types
import CAR.AnnotationsFile as AnnsFile
import CAR.Retrieve as Retrieve
import qualified CAR.RunFile as CarRun
import CAR.Utils (nubWithKey)

import Graph
import EdgeDocCorpus
import WriteRanking
import GraphExpansion
import GraphExpansionExperiments
import SimplIR.WordEmbedding
import SimplIR.WordEmbedding.Parse
import qualified SimplIR.SimpleIndex as Index
import qualified SimplIR.SimpleIndex.Models.BM25 as BM25
import ZScore



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
--                , FilePath
               , QuerySource
--                , Maybe [Method]
--                , Int
               , Index.OnDiskIndex Term EdgeDoc Int
--                , RankingType
--                , Graphset
               , [CarRun.QueryId]
--                , Maybe FilePath
               )
opts =
    (,,,,)
    <$> argument str (help "articles file" <> metavar "ANNOTATIONS FILE")
    <*> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")
--     <*> option str (short 'e' <> long "embedding" <> metavar "FILE" <> help "Glove embeddings file")
    <*> querySource
--     <*> optional methods
--     <*> option auto (long "hops" <> metavar "INT" <> help "number of hops for initial outward expansion" <> value 3)
    <*> option (Index.OnDiskIndex <$> str)
               (short 'i' <> long "index" <> metavar "INDEX" <> help "simplir edgedoc index")
--     <*> flag EntityRanking EntityPassageRanking (long "entity-psg" <> help "If set, include provenance paragraphs")
--     <*> flag Subgraph Fullgraph (long "fullgraph" <> help "Run on full graph, not use subgraph retrieval")
    <*> many (option (CarRun.QueryId . T.pack <$> str) (long "query" <> metavar "QUERY" <> help "execute only this query"))
--     <*> optional (option str (long "dot" <> metavar "FILE" <> help "export dot graph to this file"))
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


computeRankingsForQuery :: (Index.RetrievalModel Term EdgeDoc Int -> RetrievalFunction EdgeDoc)
                        -> AnnotationsFile
                        -> CarRun.QueryId  ->  [Term]
                        -> [(RetrievalFun, [(ParagraphId, Double)])]

computeRankingsForQuery
      retrieveDocs
      annsFile queryId query =
      let

        retrievalResults :: [(RetrievalFun, [(ParagraphId, Double)])]
        retrievalResults = [ (irname, retrievalResult)
                           | (irname, retrievalModel) <- retrievalModels
                           , let retrievalResult =
                                   fmap (\(ed, ld) -> (edgeDocParagraphId ed, ln ld)) $
                                   retrieveDocs retrievalModel query
                           ]

--
--         attachParagraphs :: RetrievalFun ->  [(PageId, Double)] -> [(PageId, Maybe ParagraphId, Double)]
--         attachParagraphs irname entityRanking
--           | Just results <- lookup irname retrievalResults =
--              let psgOf :: HM.HashMap PageId ParagraphId
--                  psgOf = HM.fromListWith (\x _ -> x)
--                          $ foldMap (\(EdgeDoc{..}, _) -> fmap (\x -> (x, edgeDocParagraphId)) (HS.toList edgeDocNeighbors ++ [edgeDocArticleId]))
--                          $ results
--              in fmap (\(entity, score) -> (entity, (entity `HM.lookup` psgOf), score)) entityRanking

    in retrievalResults


logMsg :: CarRun.QueryId -> RetrievalFun -> String -> IO ()
logMsg queryId method t = T.putStr $ (CarRun.unQueryId queryId)<>"\t"<>T.pack (show method)<>"\t"<>T.pack t<>"\n"




main :: IO ()
main = do
    hSetBuffering stdout LineBuffering

    (articlesFile, outputFilePrefix, querySrc, simplirIndexFilepath,
      queryRestriction) <-
        execParser $ info (helper <*> opts) mempty
    putStrLn $ "# Pages: " ++ show articlesFile
    annsFile <- AnnsFile.openAnnotations articlesFile
    siteId <- wikiSite . fst <$> readPagesFileWithProvenance articlesFile
    putStrLn $ "# Query restriction: " ++ show queryRestriction
    putStrLn $ "# Edgedoc index: "++ show simplirIndexFilepath
--
--     let seedMethod :: SeedDerivation -> IO (QueryDoc -> QueryDoc)
--         seedMethod SeedsFromLeadSection = return id
--         seedMethod (SeedsFromEntityIndex entityIndexFile) = do
--             retrieve <- retrieveEntities entityIndexFile
--             putStrLn $ "# entity index: " ++ show entityIndexFile
--             let entitiesFromIndex :: QueryDoc -> QueryDoc
--                 entitiesFromIndex qdoc =
--                     qdoc { queryDocLeadEntities = seeds }
--                   where
--                     queryTerms = textToTokens' $ queryDocQueryText qdoc
--                     seeds = HS.fromList $ map snd
--                             $ take 5 $ retrieve queryTerms
--             return entitiesFromIndex
--
--     queriesWithSeedEntities' <-
--         case querySrc of
--           QueriesFromCbor queryFile queryDeriv seedDerivation -> do
--               populateSeeds <- seedMethod seedDerivation
--               map populateSeeds . pagesToQueryDocs siteId queryDeriv
--                   <$> readPagesOrOutlinesAsPages queryFile
--
--           QueriesFromJson queryFile -> do
--               QueryDocList queriesWithSeedEntities <- either error id . Data.Aeson.eitherDecode <$> BSL.readFile queryFile
--               return queriesWithSeedEntities
--
--     queriesWithSeedEntities <-
--         if null queryRestriction
--           then return queriesWithSeedEntities'
--           else do putStrLn $ "# using only queries "<>show queryRestriction
--                   return $ filter (\q-> queryDocQueryId q `elem` queryRestriction) queriesWithSeedEntities'
--     putStrLn $ "# query count: " ++ show (length queriesWithSeedEntities)


    queries <-
        case querySrc of
          QueriesFromCbor queryFile queryDeriv _seedDeriv -> do
              pagesToQueryDocs siteId queryDeriv <$> readPagesOrOutlinesAsPages queryFile

          QueriesFromJson queryFile -> do
              QueryDocList queries <- either error id . Data.Aeson.eitherDecode <$> BSL.readFile queryFile
              return queries

--     queriesWithSeedEntities <-
--         case querySrc of
--           QueriesFromCbor queryFile queryDeriv seedDerivation -> do
--               populateSeeds <- seedMethod seedDerivation
--               map populateSeeds . pagesToQueryDocs siteId queryDeriv
--                   <$> readPagesOrOutlinesAsPages queryFile



    index <- Index.open simplirIndexFilepath
    let retrieveDocs :: Index.RetrievalModel Term EdgeDoc Int -> RetrievalFunction EdgeDoc
        retrieveDocs model = map swap . Index.score index model
    putStrLn "# opened edgedoc index"

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
            bracket (takeMVar hdl) (putMVar hdl) $ \ h ->
                TL.hPutStrLn h formatted
          where
            onError (SomeException exc) =
                putStrLn $ concat [ "error: exception while running "
                                  , show method, " on ", show queryId, ": ", show exc ]

    let subgraphExpansion =
            forM_' queries $ -- was: queriesWithSeedEntities
              \query@QueryDoc{queryDocQueryId=queryId, queryDocPageId=queryPage, queryDocLeadEntities=seedEntities} -> do
--                 when (null $ seedEntities) $
--                     T.putStr $ T.pack $ "# Query with no lead entities: "++show query++"\n"



                T.putStr $ T.pack $ "# Processing query "++ show query++": seeds=" ++ show seedEntities ++ "\n"
                let rankings :: [(RetrievalFun, [(ParagraphId,  Double)])]
                    rankings = computeRankingsForQuery retrieveDocs annsFile queryId (queryDocRawTerms query)

--                 let methodsAvailable = S.fromList (map fst rankings)
--                     badMethods
--                       | Just ms <- runMethods = S.fromList ms `S.difference` methodsAvailable
--                       | otherwise             = S.empty
--                     filterMethods
--                       | Just ms <- runMethods = (`elem` ms)
--                       | otherwise             = const True
--                 when (not $ S.null badMethods) $ putStrLn $ "\n\nwarning: unknown methods: "++show badMethods++"\n"
--                 mapM_ (uncurry $ runIrMethod queryId query)
--                     $ filter (filterMethods . fst) rankings


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
