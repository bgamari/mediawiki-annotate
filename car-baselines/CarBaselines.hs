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
import qualified Data.Binary.Serialise.CBOR as CBOR
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.DList as DList
import qualified Numeric.Log as Log

import CAR.Types
import CAR.CarExports
import CAR.RunFile as CarRun
import SimplIR.RetrievalModels.CorpusStats
import SimplIR.RetrievalModels.QueryLikelihood
import SimplIR.RetrievalModels.BM25 as BM25
import SimplIR.TopK
import SimplIR.Types
import SimplIR.Term as Term
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

newtype BagOfWords = BagOfWords (M.Map Term Int)

instance Monoid BagOfWords where
    mempty = BagOfWords mempty
    BagOfWords a `mappend` BagOfWords b = BagOfWords (M.unionWith (+) a b)

oneWord :: Term -> BagOfWords
oneWord t = BagOfWords $ M.singleton t 1

tokenize :: T.Text -> [Term]
tokenize =  textToTokens'

paraBodyTerms :: ParaBody -> [Term]
paraBodyTerms (ParaText t) = tokenize t
paraBodyTerms (ParaLink l) = tokenize $ linkAnchor l

paraBodyBag :: ParaBody -> BagOfWords
paraBodyBag = foldMap oneWord . paraBodyTerms

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
        let stats = Foldl.fold (documentTermStats Nothing) $ fmap paraBodyTerms $ foldMap paraBody paras
        writeCorpusStats outFile $ stats

writeCorpusStats :: FilePath -> CorpusStats Term -> IO ()
writeCorpusStats fname = BSL.writeFile fname . CBOR.serialise

readCorpusStats :: FilePath -> IO (CorpusStats Term)
readCorpusStats fname =
    fromMaybe (error "Error reading corpus statistics") . CBOR.deserialise <$> BSL.readFile fname

modeMergeCorpusStats :: Parser (IO ())
modeMergeCorpusStats =
    go <$> option str (long "output" <> short 'o' <> help "output corpus statistics path")
       <*> many (argument str (metavar "STATS" <> help "corpus stats files"))
  where
    go outFile indexes = do
        stats <- mapM readCorpusStats indexes
        writeCorpusStats outFile $ mconcat stats

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
                 $ fmap (\p -> let BagOfWords terms = foldMap paraBodyBag (paraBody p)
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
        outlines <- decodeCborList <$> BSL.readFile outlineFile
        corpusStats <- readCorpusStats corpusStatsFile
        idx <- DiskIdx.openOnDiskIndex diskIdx

        let termFreq = maybe 0.5 (realToFrac . termFrequency) . flip HM.lookup (corpusTerms corpusStats)
            termProb t = termFreq t / realToFrac (corpusTokenCount corpusStats)

            modelQL, modelBM25 :: ScoringModel
            modelQL queryTerms docLen docTerms =
                queryLikelihood (Dirichlet 1000 termProb)
                                (fmap (fmap realToFrac) queryTerms)
                                docLen (fmap (fmap realToFrac) docTerms)
            modelBM25 queryTerms docLen docTerms =
                BM25.bm25 BM25.sensibleParams corpusStats
                          (HS.fromList $ map fst queryTerms)
                          docLen (HM.fromList docTerms)
            model = modelBM25

        let predictStub :: Stub -> [CarRun.RankingEntry]
            predictStub = foldMap (uncurry predictSection) . stubPaths

            predictSection :: BagOfWords -> SectionPath -> [CarRun.RankingEntry]
            predictSection query sectionPath =
                    [ CarRun.RankingEntry
                          { carQueryId    = CarRun.sectionPathToQueryId sectionPath
                          , carPassage    = paraId
                          , carRank       = rank
                          , carScore      = Log.ln score
                          , carMethodName = CarRun.MethodName $ T.pack $ BS.unpack runName
                          }
                    | (rank, (paraId, score)) <- zip [1..] $ scoreQuery model idx k query
                    ]

        CarRun.writeRunFile outputFile $ foldMap predictStub outlines

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
    headingWords (SectionHeading t) = foldMap oneWord $ tokenize t

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

type ScoringModel = [(Term, Int)]  -- ^ query term frequencies
                 -> DocumentLength -- ^ document length
                 -> [(Term, Int)]  -- ^ document term frequencies
                 -> BM25.Score

scoreQuery :: ScoringModel
           -> DiskIdx.DiskIndex (ParagraphId, DocumentLength) Int
           -> Int
           -> BagOfWords
           -> [(ParagraphId, BM25.Score)]
scoreQuery model idx k (BagOfWords query) =
       map (\(Entry a b) -> (b, a))
     $ runIdentity
     $ foldProducer (Foldl.generalize $ topK k)
     $ termPostings idx (M.keys query)
    >-> cat'                      @((ParagraphId, DocumentLength), [(Term, Int)])
    >-> P.P.map (\((paraId, docLen), docTfs) ->
                   let score = model (M.assocs query) docLen docTfs
                   in Entry score paraId)
    >-> cat'                      @(Entry BM25.Score ParagraphId)
