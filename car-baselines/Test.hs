{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad.IO.Class
import Data.Profunctor
import Data.Bifunctor
import Data.Maybe (fromMaybe)
import Data.Monoid
import qualified Data.Map.Strict as M

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
import Data.Binary

import Pipes
import qualified Pipes.Prelude as P.P
import qualified Control.Foldl as Foldl
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Options.Applicative

instance Binary ParagraphId

type CarDiskIndex = DiskIdx.OnDiskIndex (ParagraphId, DocumentLength) Int

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
skeletonTerms (Section (SectionHeading t) children) =
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
        BTree.fromOrderedToFile 64 (fromIntegral $ M.size terms) outFile
                                (Pipes.each $ map toBLeaf $ M.assocs terms)

openCorpusStats :: FilePath -> IO (BTree.LookupTree Term Int)
openCorpusStats = fmap (either error id) . BTree.open

modeMergeCorpusStats :: Parser (IO ())
modeMergeCorpusStats =
    go <$> option str (long "output" <> short 'o' <> help "output corpus statistics path")
       <*> many (argument str (help "corpus stats files"))
  where
    go outFile indexes = do
        idxs <- mapM openCorpusStats indexes
        BTree.mergeTrees (\a b -> pure $! a+b) 64 outFile idxs

modeIndex :: Parser (IO ())
modeIndex =
    go <$> option str (long "output" <> short 'o' <> help "output index path")
       <*> argument str (help "annotations file")
  where
    go outFile annotationsFile = do
        anns <- openAnnotations annotationsFile
        let paras = concatMap toParagraphs (pages anns)
        Foldl.foldM (DiskIdx.buildIndex 100000 outFile)
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
  where
    go :: CarDiskIndex -> IO ()
    go diskIdx = do
        let query = map Term.fromString ["hello", "world"]
            smoothing = NoSmoothing
        idx <- DiskIdx.openOnDiskIndex diskIdx
        results <- scoreQuery smoothing idx query
        print results

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

scoreQuery :: (Monad m)
           => Smoothing Term
           -> DiskIdx.DiskIndex (ParagraphId, DocumentLength) Int
           -> [Term]
           -> m [(ParagraphId, Score)]
scoreQuery smoothing idx query =
    foldProducer (Foldl.generalize $ topK 20)
    $ termPostings idx query
    >-> cat'                      @((ParagraphId, DocumentLength), [(Term, Int)])
    >-> P.P.map (\((paraId, docLen), docTf) ->
                   let score = queryLikelihood smoothing (M.assocs queryTf) docLen $ map (second realToFrac) docTf
                   in (paraId, score))
    >-> cat'                      @(ParagraphId, Score)
  where
    queryTf =
        fmap realToFrac $ M.unionsWith (+) $ map (`M.singleton` 1) query
