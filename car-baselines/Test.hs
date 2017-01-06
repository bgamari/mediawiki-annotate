{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}

import Control.Monad.IO.Class
import Data.Foldable
import Data.Profunctor
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
import CAR.AnnotationsFile
import CAR.CarExports
import SimplIR.RetrievalModels.QueryLikelihood
import SimplIR.TopK
import SimplIR.Types
import SimplIR.Term as Term
import SimplIR.Tokenise (killPunctuation)
import SimplIR.Utils
import SimplIR.DiskIndex.Posting.Collect (collectPostings)
import qualified SimplIR.DiskIndex.Build as DiskIdx
import qualified SimplIR.DiskIndex as DiskIdx

import Pipes
import qualified Pipes.Prelude as P.P
import Pipes.Safe
import qualified Control.Foldl as Foldl
import Options.Applicative

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
tokenize = foldMap (oneWord . Term.fromText) . T.words . T.toCaseFold . killPunctuation

skeletonTerms :: PageSkeleton -> BagOfWords
skeletonTerms (Para (Paragraph _ t)) = foldMap paraBodyTerms t
skeletonTerms (Section (SectionHeading t) _ children) =
    tokenize t <> foldMap skeletonTerms children

paraBodyTerms :: ParaBody -> BagOfWords
paraBodyTerms (ParaText t) = tokenize t
paraBodyTerms (ParaLink _ anchor) = tokenize anchor

modeCorpusStats :: Parser (IO ())
modeCorpusStats =
    go <$> option str (long "output" <> short 'o' <> help "output corpus statistics path")
       <*> argument str (help "annotations file")
  where
    go outFile annotationsFile = do
        anns <- openAnnotations annotationsFile
        let BagOfWords terms = foldMap (foldMap skeletonTerms . pageSkeleton) (pages anns)
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
       <*> many (argument str (help "corpus stats files"))
  where
    go outFile indexes = do
        idxs <- mapM openTermFreqs indexes
        BTree.mergeTrees (\a b -> pure $! a+b) 64 (outFile <.> "terms") idxs
        stats <- mapM readCorpusStats indexes
        writeCorpusStats outFile $ fold stats

modeIndex :: Parser (IO ())
modeIndex =
    go <$> option str (long "output" <> short 'o' <> help "output index path")
       <*> argument str (help "annotations file")
  where
    go outFile annotationsFile = do
        anns <- openAnnotations annotationsFile
        let paras = concatMap toParagraphs (pages anns)
        runSafeT $ Foldl.foldM (DiskIdx.buildIndex 100000 outFile)
                  $ fmap (\p -> let BagOfWords terms = foldMap paraBodyTerms (paraBody p)
                                in ((paraId p, DocLength $ sum terms), terms)
                         ) paras

        return ()

modeMerge :: Parser (IO ())
modeMerge =
    go
      <$> option str (long "output" <> short 'o' <> help "output path")
      <*> many (argument (DiskIdx.OnDiskIndex <$> str) (help "annotations file"))
  where
    go :: FilePath -> [CarDiskIndex] -> IO ()
    go outPath parts = mapM DiskIdx.openOnDiskIndex parts >>= DiskIdx.merge outPath

modeQuery :: Parser (IO ())
modeQuery =
    go <$> option (DiskIdx.OnDiskIndex <$> str) (long "index" <> short 'i' <> help "Index directory")
       <*> option str (long "stats" <> short 's' <> help "Corpus statistics")
       <*> option str (long "skeletons" <> short 'S' <> help "File containing page skeletons to predict (one per line)")
       <*> option (BS.pack <$> str) (long "run" <> short 'r' <> help "The run name" <> value (BS.pack "run"))
       <*> option auto (long "count" <> short 'k' <> help "How many results to retrieve per query" <> value 1000)
  where
    go :: CarDiskIndex -> FilePath -> FilePath -> BS.ByteString -> Int -> IO ()
    go diskIdx corpusStatsFile skeletonFile runName k = do
        termFreqs <- openTermFreqs corpusStatsFile
        skeletons <- decodeCborList <$> BSL.readFile skeletonFile
        corpusStats <- readCorpusStats corpusStatsFile
        idx <- DiskIdx.openOnDiskIndex diskIdx

        let query = map Term.fromString ["hello", "world"]
            termFreq = realToFrac . fromMaybe 0 . BTree.lookup termFreqs
            termProb t = termFreq t / realToFrac (corpusNTokens corpusStats)
            smoothing = Dirichlet 1000 termProb

        let predictStub :: Stub -> IO ()
            predictStub = mapM_ (uncurry predictSection) . stubPaths

            predictSection :: BagOfWords -> SectionPath -> IO ()
            predictSection query sectionPath = do
                let results = scoreQuery smoothing idx k query
                BSL.putStrLn $ BSB.toLazyByteString
                    $ prettyTrecRun runName
                      [ (sectionPath, paraId, score)
                      | (paraId, score) <- results
                      ]

        mapM_ predictStub skeletons

-- | Format results in the TREC run file format
prettyTrecRun :: BS.ByteString -- ^ run ID
              -> [(SectionPath, ParagraphId, Score)]
              -> BSB.Builder
prettyTrecRun runName =
    mconcat . intersperse (BSB.char7 '\n') . zipWith entry [1..]
  where
    entry rank (path, ParagraphId paraId, score) =
        mconcat $ intersperse (BSB.char8 ' ')
        [ BSB.string8 $ escapeSectionPath path
        , BSB.char7 '0' -- iteration
        , BSB.shortByteString paraId
        , BSB.intDec rank
        , BSB.doubleDec $ Log.ln score
        , BSB.byteString runName
        ]

stubPaths :: Stub -> [(BagOfWords, SectionPath)]
stubPaths (Stub _ pageId skel) = foldMap (go mempty mempty) skel
  where
    go :: DList.DList HeadingId -> BagOfWords -> PageSkeleton -> [(BagOfWords, SectionPath)]
    go _ _ (Para _) = [] -- this should really never happen
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
    >-> P.P.map (\((paraId, docLen), docTf) ->
                   let score = queryLikelihood smoothing (M.assocs queryReal) docLen $ map (second realToFrac) docTf
                   in Entry score paraId)
    >-> cat'                      @(Entry Score ParagraphId)
  where queryReal = fmap realToFrac query
