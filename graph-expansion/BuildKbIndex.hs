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
import Data.Binary
import Data.Ord
import Control.Applicative
import Data.Foldable
import Data.Maybe
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Tuple
import Data.Semigroup hiding (All, Any, option)
import Data.Coerce
import qualified Data.Map.Strict as M
import qualified Data.Set as S
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
import Numeric.Log
import Data.Hashable
import Text.PrettyPrint.ANSI.Leijen hiding ((<>), (<$>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Pipes
import qualified Pipes.ByteString as PBS

import Options.Applicative
import EdgeDocCorpus
import CAR.Types
import CAR.Retrieve
import CAR.KnowledgeBase
import CAR.Utils
import CAR.Utils.Redirects
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


desc :: PP.Doc
desc = "Retrieve entities from entity index `index`." <$$>
       "Supports queries with cbor outlines or custom through JSON." <$$>
       "JSON format : " <$$>
       indent 4 (vcat [ "{\"queryDocListContent\":["
                      , "    {\"queryDocPageId\":\"pageId\""
                      , "    ,\"queryDocQueryText\":\"stemmed query text\""
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

edgeDocModes :: Parser (IO ())
edgeDocModes = subparser
    $ command "index" (info (helper <*> buildMode) mempty)
   <> command "dump" (info (helper <*> dumpMode) mempty)
   <> command "query" (info (helper <*> queryMode) mempty)
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
            print postings

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

entityModes :: Parser (IO ())
entityModes = subparser
    $ command "index" (info (helper <*> indexMode) mempty)
   <> command "warc" (info (helper <*> warcMode) mempty)
   <> command "dump" (info (helper <*> dumpMode) mempty)
   <> command "query" (info (helper <*> queryMode) mempty)
  where
    indexMode =
        go <$> option str (long "output" <> short 'o' <> help "output index path")
           <*> argument str (metavar "CBOR" <> help "kb articles file")
           <*> flag FullText LeadText (long "lead" <> help "Index only lead text (if not set, index full text)")
      where
        go outputPath articlesPath textPart = do
            (prov, pages) <- readPagesFileWithProvenance articlesPath
            let !resolveRedirect = resolveRedirects pages

            !inlinkInfo <- collectInlinkInfo (wikiSite prov) resolveRedirect <$> readPagesFile articlesPath
            pages2 <- readPagesFile articlesPath

            let docTerms :: KbDoc -> [Term]
                docTerms doc =
                       docText
                    ++ foldMap tokeniseText (HM.keys $ anchorCount inlinks)
                    ++ foldMap tokenisePageId (HM.keys $ disambiguationCount inlinks)
                  where
                    docText = case textPart of
                      FullText -> foldMap (tokeniseText . TL.toStrict) (kbDocFullText doc)
                      LeadText -> foldMap tokeniseText (kbDocLeadText doc)
                    inlinks = fromMaybe mempty
                              $ HM.lookup (kbDocPageId doc) (documentInlinks inlinkInfo)
                    tokenisePageId = textToTokens' . getPageName
                    tokeniseText   = textToTokens'

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
            let !resolveRedirect = resolveRedirects pages

            !inlinkInfo <- collectInlinkInfo (wikiSite prov) resolveRedirect <$> readPagesFile articlesPath
            pages2 <- readPagesFile articlesPath

            let docTerms :: KbDoc -> [T.Text]
                docTerms doc =
                       [docText]
                    ++ (HM.keys $ anchorCount inlinks)
                    ++ (map getPageName $ HM.keys $ disambiguationCount inlinks)
                  where
                    docText = case textPart of
                      FullText -> foldMap (TL.toStrict) (kbDocFullText doc)
                      LeadText -> fold (kbDocLeadText doc)
                    inlinks = fromMaybe mempty
                              $ HM.lookup (kbDocPageId doc) (documentInlinks inlinkInfo)
                    tokenisePageId = textToTokens' . getPageName
                    tokeniseText   = textToTokens'

            let emptyRecordHeader = Warc.RecordHeader Warc.warc0_16 mempty

            writeRecords "out.warc"
                [ Warc.Record emptyRecordHeader (mapM_ (yield . T.encodeUtf8) (docTerms doc))
                | doc <- map pageToKbDoc pages2
                , let hdr = Warc.addField Warc.warcRecordId recId emptyRecordHeader
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
   <> command "query" (info (helper <*> queryMode) mempty)
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
                [ (paraId psg, docTerms psg)
                | psg <- paragraphs
                ]
            return ()

    dumpMode =
        go <$> option (OnDiskIndex <$> str) (long "index" <> short 'i' <> help "index path")
           <*> some (argument (Term.fromString <$> str) (metavar "TERM" <> help "query terms"))
      where
        go :: Index.OnDiskIndex Term ParagraphId Int -> [Term] -> IO ()
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
    mode <- execParser $ info (helper <*> modes) mempty
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



queryIndexToTrecRun :: forall t. Binary t => (t -> TrecRun.DocumentName) -> Index.OnDiskIndex Term t Int -> QuerySource -> FilePath -> HS.HashSet TrecRun.QueryId->  T.Text -> Int -> T.Text -> IO ()
queryIndexToTrecRun getDocName indexFile querySource output selectedQueries methodName topK retrievalModelName = do
    index <- Index.open indexFile
    let retrieve = sortBy Index.descending . Index.score index retrievalModel
         where retrievalModel = case retrievalModelName of
                                  "bm25" -> bm25
                                  "ql" -> ql



    queries <-
        case querySource of
          QueriesFromCbor queryFile queryDeriv -> do
              pagesToQueryDocs queryDeriv <$> readPagesFile queryFile

          QueriesFromJson queryFile -> do
              QueryDocList queries <- either error id . Data.Aeson.eitherDecode <$> BSL.readFile queryFile
              return queries


    let retrieveQuery :: QueryDoc -> [TrecRun.RankingEntry]
        retrieveQuery QueryDoc{..} =
            fmap toTrecEntry
                $ zip [1.. ]
                $ take topK
                $ retrieve (textToTokens' queryDocQueryText)
          where toTrecEntry::  (Int, (Log Double, t)) -> TrecRun.RankingEntry
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
