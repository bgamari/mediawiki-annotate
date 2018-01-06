{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ApplicativeDo #-}

import GHC.Generics
import Data.List
import Codec.Serialise
import Data.Ord
import Control.Applicative
import Control.Parallel.Strategies
import Data.Foldable
import Data.Maybe
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Tuple
import Data.Semigroup hiding (All, Any, option)
import Data.Coerce
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import qualified Data.SmallUtf8 as Utf8
import System.IO
import Numeric
import GHC.TypeLits
import Data.Aeson
import Numeric.Log hiding (sum)
import Data.Hashable
import Text.PrettyPrint.ANSI.Leijen hiding ((<>), (<$>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import qualified Control.Foldl as Foldl
import Data.Foldable (foldl')


import Pipes
import qualified Pipes.ByteString as PBS

import Options.Applicative
import EdgeDocCorpus
import CAR.Types
import CAR.Retrieve
import CAR.KnowledgeBase
import CAR.Utils
import CAR.ToolVersion
import CAR.Utils.Redirects
import SimplIR.Utils
import SimplIR.Term as Term
import SimplIR.SimpleIndex as Index
import Data.Warc as Warc

import CAR.AnnotationsFile as AnnsFile
import qualified CAR.RunFile as CarRun
import qualified SimplIR.SimpleIndex as Index
import qualified SimplIR.SimpleIndex.Models.QueryLikelihood as QL
import qualified SimplIR.SimpleIndex.Models.BM25 as BM25

import qualified SimplIR.Format.TrecRunFile as TrecRun

import qualified Data.Text.Lazy as TL

data TextPart = FullText | LeadText
data LinkMode = NoLinks | WithLinks


type EntityIndex = Index.OnDiskIndex Term PageId Int


data QuerySource = QueriesFromCbor FilePath QueryDerivation
                 | QueriesFromJson FilePath

data QueryDerivation = QueryFromPageTitle | QueryFromSectionPaths


queryHelpDesc :: PP.Doc
queryHelpDesc = "Retrieve entities from entity index `index`." <$$>
       "Supports queries with cbor outlines or custom through JSON." <$$>
       "JSON format : " <$$>
       indent 4 (vcat [ "{\"queryDocListContent\":["
                      , "    {\"queryDocPageId\":\"pageId\""
                      , "    ,\"queryDocQueryText\":\"query text\""
                      , "    ,\"queryDocQueryId\":\"qidForRun\"}"
                      , ",...]}"
                      ])

data Opts = Opts { output :: FilePath
                 , query :: QuerySource
                 , selectedQueries :: HS.HashSet TrecRun.QueryId
                 , entityIndexFile :: Index.OnDiskIndex Term PageId Int
                 , topK :: Int
                 , methodName :: T.Text
                 }

data QueryDoc = QueryDoc { queryDocQueryId      :: !TrecRun.QueryId
                         , queryDocPageId       :: !PageId
                         , queryDocQueryText    :: !T.Text
                         }
           deriving (Show, Generic)
instance FromJSON QueryDoc
instance ToJSON QueryDoc

data QueryDocList = QueryDocList { queryDocListContent :: [QueryDoc]}
           deriving Generic
instance FromJSON QueryDocList
instance ToJSON QueryDocList


querySource :: Parser QuerySource
querySource =
        fromCbor
    <|> fromJson
  where
    fromCbor =
          QueriesFromCbor
          <$> option str (short 'q' <> long "queries" <> metavar "CBOR" <> help "Queries from CBOR pages")
          <*> flag QueryFromPageTitle QueryFromSectionPaths
                 (long "query-from-sections" <> help "Use sections as query documents")

    fromJson =
        QueriesFromJson
          <$> option str (short 'j' <> long "queries-json" <> metavar "JSON" <> help "Queries from JSON")


writeRecords :: FilePath -> [Record IO ()] -> IO ()
writeRecords path recs = withFile path WriteMode $ \hdl ->
    runEffect $ mapM_ encodeRecord recs >-> PBS.toHandle hdl

pageIdToRecordId :: PageId -> Warc.RecordId
pageIdToRecordId = RecordId . Uri . Utf8.toByteString . unPageId

-- dsxyz
edgeDocModes :: Parser (IO ())
edgeDocModes = subparser
    $ command "index" (info (helper <*> buildMode) mempty)
   <> command "dump" (info (helper <*> dumpMode) mempty)
   <> command "query" (info (helper <*> queryMode) (progDescDoc (Just queryHelpDesc)))
  where
    buildMode =
        go <$> option str (long "output" <> short 'o' <> help "output index path")
           <*> argument str (metavar "CBOR" <> help "kb articles file")
      where
        go outputPath articlesPath = do
            pages <- readPagesFile articlesPath
            Index.buildTermFreq outputPath
                [ (edoc, textToTokens' $ edgeDocContent edoc)
                | edoc <- pagesToEdgeDocs pages
                ]
            return ()

    dumpMode =
        go <$> option (OnDiskIndex <$> str) (long "index" <> short 'i' <> help "index path")
           <*> some (argument (Term.fromString <$> str) (metavar "TERM" <> help "query terms"))
      where
        go :: Index.OnDiskIndex Term EdgeDoc Int -> [Term] -> IO ()
        go indexPath terms = do
            idx <- Index.open indexPath
            let postings = fold $ map (Index.lookupPostings idx) terms
            mapM_ print $ sortBy (flip $ comparing snd) postings

    queryMode =
        go <$> option (OnDiskIndex <$> str) (long "index" <> short 'i' <> help "index path")
           <*> querySource
           <*> option str (short 'o' <> long "output" <> help "output run filename")
           <*> (HS.fromList <$> many (option (T.pack <$> str) (long "query" <> metavar "QUERY" <> help "execute only this query (option can be included multiple times)")))
           <*> (T.pack <$> option str (short 'n' <> value "" <> long "methodname" <> metavar "NAME" <> help "name of method for trec run file" ))
           <*> option auto (short 'k' <> value 100 <> long "topK" <> help "number of ranking entries per query" <> metavar "K" )
           <*> option str (short 'm' <> long "retrievalmodel" <> help "retrieval model (bm25 or ql)" )
      where
        go :: Index.OnDiskIndex Term EdgeDoc Int
                -> QuerySource
                -> FilePath
                -> HS.HashSet TrecRun.QueryId
                -> T.Text
                -> Int
                -> String
                -> IO ()
        go indexFile queries output selectedQuery methodName topK retrievalModelName = do
            queryIndexToTrecRun getDocName indexFile queries output selectedQuery methodName topK (T.pack retrievalModelName)
              where getDocName = T.pack . unpackParagraphId . edgeDocParagraphId

-- Todo  pageNameResolver: merge with FillMetaData
buildPageNameMap :: Foldl.Fold Page (HM.HashMap PageId PageName)
buildPageNameMap =
    Foldl.Fold (\acc page -> HM.insert (pageId page) (pageName page) acc) mempty id



entityModes :: Parser (IO ())
entityModes = subparser
    $ command "index" (info (helper <*> indexMode) mempty)
   <> command "warc" (info (helper <*> warcMode) mempty)
   <> command "dump" (info (helper <*> dumpMode) mempty)
   <> command "query" (info (helper <*> queryMode) (progDescDoc (Just queryHelpDesc)))
  where
    indexMode =
        go <$> option str (long "output" <> short 'o' <> help "output index path")
           <*> argument str (metavar "CBOR" <> help "kb articles file")
           <*> flag FullText LeadText (long "lead" <> help "Index only lead text (if not set, index full text)")
      where
        go outputPath articlesPath textPart = do
            (prov, pages) <- readPagesFileWithProvenance articlesPath
            -- Todo  pageNameResolver: Use from FillMetaData
            pageNameMap <- Foldl.fold buildPageNameMap <$> readPagesFile articlesPath
            let pageNameResolver ::  (PageId -> Maybe PageName)
                pageNameResolver = flip HM.lookup pageNameMap

--             !inlinkInfo <- collectInlinkInfo <$> readPagesFile articlesPath
            pages2 <- readPagesFile articlesPath

            let docTerms :: KbDoc -> [Term]
                docTerms doc =
                       docText
                    ++ foldMap tokeniseText (anchorNames)
                    ++ foldMap tokenisePageName (disambiguationNames)
                    ++ foldMap tokenisePageName (inlinkPagenames)
                  where

                    docText = case textPart of
                      FullText -> foldMap (tokeniseText . TL.toStrict) (kbDocFullText doc)
                      LeadText -> foldMap tokeniseText (kbDocLeadText doc)
--                     inlinks = fromMaybe mempty
--                               $ HM.lookup (kbDocPageId doc) (documentInlinks inlinkInfo)
                    tokenisePageName = textToTokens' . getPageName
                    tokeniseText   = textToTokens'
                    anchorNames :: [T.Text]
                    anchorNames = fromMaybe mempty $ fmap (map fst . toList)
                                  $ getMetadata _InlinkAnchors $ kbDocMetadata $ doc
                    disambiguationNames :: [PageName]
                    disambiguationNames =  fromMaybe mempty $ getMetadata _DisambiguationNames $ kbDocMetadata $ doc
                    inlinkPagenames :: [PageName]
                    inlinkPagenames =  catMaybes $ fmap pageNameResolver $ fromMaybe mempty $ getMetadata _InlinkIds $ kbDocMetadata $ doc


            Index.buildTermFreq outputPath
                [ (kbDocPageId doc, docTerms doc)
                | doc <- map pageToKbDoc pages2
                ]
            return ()

    warcMode =
        go <$> option str (long "output" <> short 'o' <> help "output WARC path")
           <*> argument str (metavar "CBOR" <> help "kb articles file")
           <*> flag FullText LeadText (long "lead" <> help "Index only lead text (if not set, index full text)")
      where
        go outputPath articlesPath textPart = do
            (prov, pages) <- readPagesFileWithProvenance articlesPath

--             !inlinkInfo <- collectInlinkInfo <$> readPagesFile articlesPath
            pages2 <- readPagesFile articlesPath

            let docTerms :: KbDoc -> [T.Text]
                docTerms doc =
                       [docText]
                    ++ (anchorNames)
                    ++ (disambiguationNames)
                    ++ ["\n\n"]
                  where
                    docText = case textPart of
                      FullText -> T.unwords $ fmap (TL.toStrict) (kbDocFullText doc)
                      LeadText -> T.unwords $ (kbDocLeadText doc)

                    anchorNames :: [T.Text]
                    anchorNames = fromMaybe mempty $ fmap (map fst . toList)
                                  $ getMetadata _InlinkAnchors $ kbDocMetadata $ doc
                    disambiguationNames :: [T.Text]
                    disambiguationNames = map getPageName $ fromMaybe mempty $ getMetadata _DisambiguationNames $ kbDocMetadata $ doc

--                     inlinks = fromMaybe mempty
--                               $ pagemetaInlinkIds $ kbDocMetadata $ doc

            let emptyRecordHeader = Warc.RecordHeader Warc.warc0_16 mempty

            writeRecords outputPath
                [ Warc.Record hdr (yield payload)
                | doc <- map pageToKbDoc pages2
                , let hdr = Warc.addField Warc.warcRecordId recId
                            $ Warc.addField Warc.contentLength (fromIntegral len)
                            $ emptyRecordHeader
                      len = BS.length payload
                      payload = T.encodeUtf8 $ T.unwords $ docTerms doc
                      recId = pageIdToRecordId $ kbDocPageId doc
                ]
            return ()

    dumpMode =
        go <$> option (OnDiskIndex <$> str) (long "index" <> short 'i' <> help "index path")
           <*> some (argument (Term.fromString <$> str) (metavar "TERM" <> help "query terms"))
      where
        go :: Index.OnDiskIndex Term PageId Int -> [Term] -> IO ()
        go indexPath terms = do
            idx <- Index.open indexPath
            let postings = fold $ map (Index.lookupPostings idx) terms
            mapM_ print $ sortBy (flip $ comparing snd) postings

    queryMode =
        go <$> option (OnDiskIndex <$> str) (long "index" <> short 'i' <> help "index path")
           <*> querySource
           <*> option str (short 'o' <> long "output" <> help "output run filename")
           <*> (HS.fromList <$> many (option (T.pack <$> str) (long "query" <> metavar "QUERY" <> help "execute only this query (option can be included multiple times)")))
           <*> (T.pack <$> option str (short 'n' <> value "" <> long "methodname" <> metavar "NAME" <> help "name of method for trec run file" ))
           <*> option auto (short 'k' <> value 100 <> long "topK" <> help "number of ranking entries per query" <> metavar "K" )
           <*> option str (short 'm' <> long "retrievalmodel" <> help "retrieval model (bm25 or ql)")
      where
        go :: Index.OnDiskIndex Term PageId Int
                -> QuerySource
                -> FilePath
                -> HS.HashSet TrecRun.QueryId
                -> T.Text
                -> Int
                -> String
                -> IO ()
        go indexFile queries output selectedQuery methodName topK retrievalModelName = do
            queryIndexToTrecRun getDocName indexFile queries output selectedQuery methodName topK (T.pack retrievalModelName)
              where getDocName = T.pack . unpackPageId


paragraphModes :: Parser (IO ())
paragraphModes = subparser
    $ command "index" (info (helper <*> indexMode) mempty)
   <> command "dump" (info (helper <*> dumpMode) mempty)
   <> command "query" (info (helper <*> queryMode) (progDescDoc (Just queryHelpDesc)))
  where
    indexMode =
        go <$> option str (long "output" <> short 'o' <> help "output index path")
           <*> argument str (metavar "CBOR" <> help "paragraph file")
           <*> flag NoLinks WithLinks (long "links" <> help "Index also link targets ")
      where
        go outputPath paragraphsPath textPart = do

            paragraphs <- readParagraphsFile paragraphsPath

            let docTerms :: Paragraph -> [Term]
                docTerms paragraph =
                       (tokeniseText $ TL.toStrict $ paraToText paragraph)
                    ++ outlinkText
                  where
                    outlinkText = case textPart of
                      NoLinks -> mempty
                      WithLinks -> foldMap (tokeniseText . getPageName . linkTarget) $ paraLinks paragraph
                    tokeniseText   = textToTokens'

            Index.buildTermFreq outputPath
                $ statusList 1000 (\n->"paragraph"++show n)
                $ withStrategy (parBuffer 128 rdeepseq)
                [ (paraId psg, {-# SCC "docTerms" #-}docTerms psg)
                | psg <- paragraphs
                ]
            return ()

    dumpMode =
        go <$> option (OnDiskIndex <$> str) (long "index" <> short 'i' <> help "index path")
           <*> flag (\s -> [Term.fromText s]) textToTokens' (long "tokenise" <> short 'T' <> help "Tokenise terms")
           <*> some (argument (T.pack <$> str) (metavar "TERM" <> help "query terms"))
      where
        go :: Index.OnDiskIndex Term ParagraphId Int -> (T.Text -> [Term]) -> [T.Text] -> IO ()
        go indexPath toTokens terms = do
            hPutStrLn stderr "building index" >> hFlush stderr
            idx <- Index.open indexPath
            hPutStrLn stderr "done building index" >> hFlush stderr
            let postings = fold $ map (Index.lookupPostings idx) $ foldMap toTokens terms
            mapM_ print $ postings -- $ sortBy (flip $ comparing snd) postings

    queryMode =
        go <$> option (OnDiskIndex <$> str) (long "index" <> short 'i' <> help "index path")
           <*> querySource
           <*> option str (short 'o' <> long "output" <> help "output run filename")
           <*> (HS.fromList <$> many (option (T.pack <$> str) (long "query" <> metavar "QUERY" <> help "execute only this query (option can be included multiple times)")))
           <*> (T.pack <$> option str (short 'n' <> value "" <> long "methodname" <> metavar "NAME" <> help "name of method for trec run file" ))
           <*> option auto (short 'k' <> value 100 <> long "topK" <> help "number of ranking entries per query" <> metavar "K" )
           <*> option str (short 'm' <> long "retrievalmodel" <> help "retrieval model (bm25 or ql)")
      where
        go :: Index.OnDiskIndex Term ParagraphId Int
                -> QuerySource
                -> FilePath
                -> HS.HashSet TrecRun.QueryId
                -> T.Text
                -> Int
                -> String
                -> IO ()
        go indexFile queries output selectedQuery methodName topK retrievalModelName = do
            queryIndexToTrecRun getDocName indexFile queries output selectedQuery methodName topK (T.pack retrievalModelName)
              where getDocName = T.pack . unpackParagraphId

modes = subparser
    $ command "entity"  (info (helper <*> entityModes) mempty)
   <> command "edgedoc" (info (helper <*> edgeDocModes) mempty)
   <> command "paragraph" (info (helper <*> paragraphModes) mempty)

main :: IO ()
main = do
    mode <- execParser' 1 (helper <*> modes) mempty
    mode




bm25 :: (Hashable term, Eq term) => Index.RetrievalModel term doc Int
bm25 = BM25.bm25 $ BM25.sensibleParams

ql :: (Hashable term, Eq term) => Index.RetrievalModel term doc Int
ql = QL.queryLikelihood (QL.Dirichlet 100)

pagesToQueryDocs :: QueryDerivation -> [Page] ->  [QueryDoc]
pagesToQueryDocs deriv pages  =
    queryDocs
  where
    queryDocs = case deriv of
      QueryFromPageTitle ->
          [ QueryDoc { queryDocQueryId      = T.pack $ unpackPageId $ pageId page
                     , queryDocPageId       = pageId page
                     , queryDocQueryText    = getPageName $ pageName page
                     }
          | page <- pages
         ]
      QueryFromSectionPaths ->
          [ QueryDoc { queryDocQueryId      = T.pack $ escapeSectionPath sectionPath
                     , queryDocPageId       = pageId page
                     , queryDocQueryText    = T.unwords
                                            $ getPageName (pageName page) : getPageName (pageName page) -- twice factor
                                              : map getSectionHeading headings
                     }
          | page <- pages
          , (sectionPath, headings, _) <- pageSections page
          ]

queryDocRawTerms :: QueryDoc -> [Term]
queryDocRawTerms = textToTokens' . queryDocQueryText



queryIndexToTrecRun :: forall t. (Serialise t, NFData t)
                    => (t -> TrecRun.DocumentName)
                    -> Index.OnDiskIndex Term t Int
                    -> QuerySource
                    -> FilePath    -- ^ output path
                    -> HS.HashSet TrecRun.QueryId
                    -> TrecRun.MethodName
                    -> Int         -- ^ k
                    -> T.Text      -- ^ retrieval model name
                    -> IO ()
queryIndexToTrecRun getDocName indexFile querySource output selectedQueries methodName topK retrievalModelName = do
    index <- Index.open indexFile
    let retrieve = sortBy Index.descending . Index.score index retrievalModel
         where retrievalModel = case retrievalModelName of
                                  "bm25" -> bm25
                                  "ql" -> ql
    queries <-
        case querySource of
          QueriesFromCbor queryFile queryDeriv -> do
              pagesToQueryDocs queryDeriv <$> readPagesOrOutlinesAsPages queryFile

          QueriesFromJson queryFile -> do
              QueryDocList queries <- either error id . Data.Aeson.eitherDecode <$> BSL.readFile queryFile
              return queries

    let retrieveQuery :: QueryDoc -> [TrecRun.RankingEntry]
        retrieveQuery QueryDoc{..} =
            fmap toTrecEntry
                $ zip [1.. ]
                $ take topK
                $ retrieve (textToTokens' queryDocQueryText)
          where toTrecEntry :: (Int, (Log Double, t)) -> TrecRun.RankingEntry
                toTrecEntry (rank, (score, doc)) =
                    TrecRun.RankingEntry {
                                   queryId  = queryDocQueryId
                                 , documentName  = getDocName doc
                                 , documentRank  = rank
                                 , documentScore = ln score
                                 , methodName    = methodName
                                 }

    let isSelectedQuery query =
            null selectedQueries ||
                (queryDocQueryId query) `HS.member` selectedQueries

    let trecRanking = foldMap retrieveQuery $ filter isSelectedQuery queries
    TrecRun.writeRunFile output trecRanking
