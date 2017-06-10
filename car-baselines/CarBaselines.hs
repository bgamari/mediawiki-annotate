{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.Foldable
import Data.Bifunctor
import Data.Functor.Identity
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Data.Monoid
import GHC.Generics
import System.FilePath

import Data.Binary
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import qualified Data.DList as DList
import qualified Numeric.Log as Log

import qualified BTree
import CAR.Types
import CAR.CarExports
import SimplIR.RetrievalModels.QueryLikelihood
import SimplIR.TopK
import SimplIR.Types
import SimplIR.Term as Term
import qualified SimplIR.Format.TrecRunFile as TrecRun
import SimplIR.Utils
import SimplIR.DiskIndex.Posting.Collect (collectPostings)
import qualified SimplIR.DiskIndex.Build as DiskIdx
import qualified SimplIR.DiskIndex as DiskIdx

import Pipes
import qualified Pipes.Prelude as P.P
import Pipes.Safe
import qualified Control.Foldl as Foldl
import Options.Applicative

import CAR.Retrieve (textToTokens')

instance Binary ParagraphId

type CarDiskIndex = DiskIdx.OnDiskIndex (ParagraphId, DocumentLength) Int

data CorpusStats = CorpusStats { corpusNTokens :: !Int }
                 deriving (Generic)
instance Monoid CorpusStats where
    mempty = CorpusStats 0
    a `mappend` b = CorpusStats { corpusNTokens = corpusNTokens a + corpusNTokens b }
instance Aeson.FromJSON CorpusStats
instance Aeson.ToJSON CorpusStats

type TermFreqIndex = BTree.LookupTree Term Int
newtype BagOfWords = BagOfWords (M.Map Term Int)

instance Monoid BagOfWords where
    mempty = BagOfWords mempty
    BagOfWords a `mappend` BagOfWords b = BagOfWords (M.unionWith (+) a b)

oneWord :: Term -> BagOfWords
oneWord t = BagOfWords $ M.singleton t 1

tokenize :: T.Text -> BagOfWords
tokenize =  foldMap oneWord . textToTokens'

paraBodyTerms :: ParaBody -> BagOfWords
paraBodyTerms (ParaText t) = tokenize t
paraBodyTerms (ParaLink l) = tokenize $ linkAnchor l

modeCorpusStats :: Parser (IO ())
modeCorpusStats =
    go <$> option str (long "output" <> short 'o' <> help "output corpus statistics path")
       <*> optional (option auto (long "first-n" <> short 'n' <> help "only build statistics from first-N paragraphs"))
       <*> argument str (metavar "PARAGRAPHS" <> help "paragraphs file")
  where
    go outFile firstN paragraphsFile = do
        let maybeTakeN = maybe id take firstN
        paras <- maybeTakeN <$> readCborList paragraphsFile
            :: IO [Paragraph]
        let BagOfWords terms = foldMap (foldMap paraBodyTerms . paraBody) paras
            toBLeaf (a,b) = BTree.BLeaf a b
        BTree.fromOrderedToFile 64 (fromIntegral $ M.size terms) (outFile <.> "terms")
                                (Pipes.each $ map toBLeaf $ M.assocs terms)
        writeCorpusStats outFile $ CorpusStats (sum terms)

openTermFreqs :: FilePath -> IO TermFreqIndex
openTermFreqs = fmap (either error id) . BTree.open . (<.> "terms")

writeCorpusStats :: FilePath -> CorpusStats -> IO ()
writeCorpusStats fname = BSL.writeFile fname . Aeson.encode

readCorpusStats :: FilePath -> IO CorpusStats
readCorpusStats fname =
    fromMaybe (error "Error reading corpus statistics") . Aeson.decode <$> BSL.readFile fname

modeMergeCorpusStats :: Parser (IO ())
modeMergeCorpusStats =
    go <$> option str (long "output" <> short 'o' <> help "output corpus statistics path")
       <*> many (argument str (metavar "STATS" <> help "corpus stats files"))
  where
    go outFile indexes = do
        idxs <- mapM openTermFreqs indexes
        BTree.mergeTrees (\a b -> pure $! a+b) 64 (outFile <.> "terms") idxs
        stats <- mapM readCorpusStats indexes
        writeCorpusStats outFile $ fold stats

modeIndex :: Parser (IO ())
modeIndex =
    go <$> option str (long "output" <> short 'o' <> help "output index path")
       <*> argument str (metavar "PARAS" <> help "paragraphs file")
  where
    go outFile paragraphsFile = do
        paras <- readCborList paragraphsFile
              :: IO [Paragraph]
        runSafeT $ Foldl.foldM (DiskIdx.buildIndex 100000 outFile)
                 $ statusList 1000 (\n -> show n <> " paragraphs")
                 $ fmap (\p -> let BagOfWords terms = foldMap paraBodyTerms (paraBody p)
                               in ((paraId p, DocLength $ sum terms), terms)
                        ) paras

        return ()

modeMerge :: Parser (IO ())
modeMerge =
    go
      <$> option str (long "output" <> short 'o' <> help "output path")
      <*> many (argument (DiskIdx.OnDiskIndex <$> str) (metavar "INDEX" <> help "index file"))
  where
    go :: FilePath -> [CarDiskIndex] -> IO ()
    go outPath parts = mapM DiskIdx.openOnDiskIndex parts >>= DiskIdx.merge outPath

modeQuery :: Parser (IO ())
modeQuery =
    go <$> option (DiskIdx.OnDiskIndex <$> str) (long "index" <> short 'i' <> help "Index directory")
       <*> option str (long "stats" <> short 's' <> help "Corpus statistics")
       <*> option str (long "outlines" <> short 'O' <> help "File containing page outlines to predict (one per line)")
       <*> option (BS.pack <$> str) (long "run" <> short 'r' <> help "The run name" <> value (BS.pack "run"))
       <*> option auto (long "count" <> short 'k' <> help "How many results to retrieve per query" <> value 1000)
       <*> option str (long "output" <> short 'o' <> help "Output ranking file")
  where
    go :: CarDiskIndex -> FilePath -> FilePath -> BS.ByteString -> Int -> FilePath -> IO ()
    go diskIdx corpusStatsFile outlineFile runName k outputFile = do
        termFreqs <- openTermFreqs corpusStatsFile
        outlines <- decodeCborList <$> BSL.readFile outlineFile
        corpusStats <- readCorpusStats corpusStatsFile
        idx <- DiskIdx.openOnDiskIndex diskIdx

        let termFreq = maybe 0.5 realToFrac . BTree.lookup termFreqs
            termProb t = termFreq t / realToFrac (corpusNTokens corpusStats)
            smoothing = Dirichlet 1000 termProb

        let predictStub :: Stub -> [TrecRun.RankingEntry]
            predictStub = foldMap (uncurry predictSection) . stubPaths

            predictSection :: BagOfWords -> SectionPath -> [TrecRun.RankingEntry]
            predictSection query sectionPath =
                    [ TrecRun.RankingEntry
                      { TrecRun.queryId = T.pack $ escapeSectionPath sectionPath
                      , TrecRun.documentName = T.pack $ unpackParagraphId paraId
                      , TrecRun.documentRank = rank
                      , TrecRun.documentScore = Log.ln score
                      , TrecRun.methodName = T.pack $ BS.unpack runName
                      }
                    | (rank, (paraId, score)) <- zip [1..] $ scoreQuery smoothing idx k query
                    ]

        TrecRun.writeRunFile outputFile $ foldMap predictStub outlines

stubPaths :: Stub -> [(BagOfWords, SectionPath)]
stubPaths (Stub _ pageId skel) = foldMap (go mempty mempty) skel
  where
    go :: DList.DList HeadingId -> BagOfWords -> PageSkeleton -> [(BagOfWords, SectionPath)]
    go _ _ (Para _) = [] -- this should really never happen
    go _ _ (Image {}) = [] -- this should really never happen
    go parents parentTerms (Section heading headingId children) =
        (terms, SectionPath pageId (toList me)) : foldMap (go me terms) children
      where
        terms = parentTerms <> headingWords heading
        me = parents `DList.snoc` headingId

    headingWords :: SectionHeading -> BagOfWords
    headingWords (SectionHeading t) = tokenize t

modes :: Parser (IO ())
modes = subparser
    $  command "corpus-stats" (info modeCorpusStats fullDesc)
    <> command "merge-corpus-stats" (info modeMergeCorpusStats fullDesc)
    <> command "index" (info modeIndex fullDesc)
    <> command "merge" (info modeMerge fullDesc)
    <> command "query" (info modeQuery fullDesc)

main :: IO ()
main = do
    mode <- execParser $ info (helper <*> modes) fullDesc
    mode

termPostings :: (Monad m, Ord p, Binary docmeta, Binary p)
             => DiskIdx.DiskIndex docmeta p
             -> [Term]
             -> Producer (docmeta, [(Term, p)]) m ()
termPostings idx terms =
    let postings = map (\term -> ( term
                                 , each $ fromMaybe [] $ DiskIdx.lookupPostings term idx)
                       ) terms
        lookupMeta (docId, docTerms) =
            case docId `DiskIdx.lookupDoc` idx of
              Nothing      -> Nothing
              Just docMeta -> Just (docMeta, docTerms)
    in collectPostings postings >-> P.P.mapFoldable lookupMeta

scoreQuery :: Smoothing Term
           -> DiskIdx.DiskIndex (ParagraphId, DocumentLength) Int
           -> Int
           -> BagOfWords
           -> [(ParagraphId, Score)]
scoreQuery smoothing idx k (BagOfWords query) =
       map (\(Entry a b) -> (b, a))
     $ runIdentity
     $ foldProducer (Foldl.generalize $ topK k)
     $ termPostings idx (M.keys query)
    >-> cat'                      @((ParagraphId, DocumentLength), [(Term, Int)])
    >-> P.P.map (\((paraId, docLen), docTfs) ->
                   let score = queryLikelihood smoothing (M.assocs queryReal) docLen
                               $ map (second realToFrac) docTfs
                   in Entry score paraId)
    >-> cat'                      @(Entry Score ParagraphId)
  where queryReal :: M.Map Term Double
        queryReal = fmap realToFrac query
