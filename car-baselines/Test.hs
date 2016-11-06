{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad.IO.Class
import Data.Profunctor
import Data.Bifunctor
import Data.Maybe (fromMaybe)

import CAR.Types
import CAR.AnnotationsFile
import CAR.CarExports
import SimplIR.RetrievalModels.QueryLikelihood
import SimplIR.TopK
import SimplIR.Types
import SimplIR.Term as Term
import SimplIR.Utils
import SimplIR.DiskIndex.Posting.Collect (collectPostings)
import Data.Binary

import Pipes
import qualified Pipes.Prelude as P.P
import qualified Control.Foldl as Foldl
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified SimplIR.DiskIndex as DiskIdx

instance Binary ParagraphId

paragraphTerms :: Paragraph -> M.Map Term Int
paragraphTerms p =
    M.unionsWith (+)
    $ map (`M.singleton` 1)
    $ foldMap paraBodyTerms
    $ paraBody p
  where
    paraBodyTerms (ParaText t) = map (Term.fromText . T.toCaseFold) $ T.words t
    paraBodyTerms (ParaLink _ t) = map (Term.fromText . T.toCaseFold) $ T.words t

main :: IO ()
main = do
    buildIndex "hello.cbor"

query :: DiskIdx.OnDiskIndex ParagraphId Int -> IO ()
query diskIdx = do
    idx <- DiskIdx.openOnDiskIndex diskIdx
    let query = ["hello", "world"]
    return ()

termPostings :: (Monad m)
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
           -> [Term]
           -> [(ParagraphId, Score)]
scoreQuery smoothing idx query =
    foldProducer (Foldl.generalize $ topK 20)
    $ termPostings idx query
    >-> cat'                      @(ParagraphId, [(Term, Int)])
    >-> P.P.map (second $ \docTf -> queryLikelihood smoothing queryTf docLength $ map (second realToFrac) docTf)
    >-> cat'                      @(ParagraphId, Score)

buildIndex :: FilePath -> IO ()
buildIndex annotationsFile = do
    anns <- openAnnotations annotationsFile
    let paras = concatMap toParagraphs (pages anns)
    chunks <- Foldl.foldM 
              $ fmap (\p -> let terms = paragraphTerms p
                            in ((paraId p, DocLength $ sum terms), terms)
                     ) paras
    putStrLn $ "Merging "++show (length chunks)++" index chunks"
    mapM DiskIdx.openOnDiskIndex chunks >>= DiskIdx.merge "paragraphs.index"

mergeIndexes :: forall docmeta p. (Binary docmeta, Binary p)
             => Foldl.FoldM IO ([(DocumentId, docmeta)], M.Map Term [Posting p])
                               [DiskIdx.OnDiskIndex docmeta p]
mergeIndexes =
    zipFoldM 0 succ
    $ premapM' chunkToIndex
    $ Foldl.generalize Foldl.list
  where
    chunkToIndex :: (Int, ([(DocumentId, docmeta)], M.Map Term [Posting p]))
                 -> IO (DiskIdx.OnDiskIndex docmeta p)
    chunkToIndex (partN, (docIdx, postingIdx)) = do
        DiskIdx.fromDocuments path docIdx postingIdx
        return (DiskIdx.OnDiskIndex path)
      where
        path = "part-"++show partN++".index"

collectIndex :: forall p docmeta.
              Foldl.Fold (docmeta, M.Map Term p)
                         ([(DocumentId, docmeta)], M.Map Term [Posting p])
collectIndex =
    zipFold (DocId 0) succ
    ((,) <$> docIdx <*> termIdx)
  where
    docIdx :: Foldl.Fold (DocumentId, (docmeta, M.Map Term p)) ([(DocumentId, docmeta)])
    docIdx =
        lmap (\(docId, (meta, _)) -> (docId, meta)) Foldl.list

    termIdx :: Foldl.Fold (DocumentId, (docmeta, M.Map Term p)) (M.Map Term [Posting p])
    termIdx =
        lmap (\(docId, (_, terms)) -> foldMap (toPosting docId) $ M.toList terms) Foldl.mconcat

    toPosting :: DocumentId -> (Term, p) -> M.Map Term [Posting p]
    toPosting docId (term, p) = M.singleton term $ [Posting docId p]

zipFoldM :: forall i m a b. Monad m
         => i -> (i -> i)
         -> Foldl.FoldM m (i, a) b
         -> Foldl.FoldM m a b
zipFoldM idx0 succ (Foldl.FoldM step0 initial0 extract0) =
    Foldl.FoldM step initial extract
  where
    initial = do s <- initial0
                 return (idx0, s)
    extract = extract0 . snd
    step (!idx, s) x = do
        s' <- step0 s (idx, x)
        return (succ idx, s')

zipFold :: forall i a b.
           i -> (i -> i)
        -> Foldl.Fold (i, a) b
        -> Foldl.Fold a b
zipFold idx0 succ (Foldl.Fold step0 initial0 extract0) =
    Foldl.Fold step initial extract
  where
    initial = (idx0, initial0)
    extract = extract0 . snd
    step (!idx, s) x =
        let s' = step0 s (idx, x)
        in (succ idx, s')

premapM' :: Monad m
         => (a -> m b)
         -> Foldl.FoldM m b c
         -> Foldl.FoldM m a c
premapM' f (Foldl.FoldM step0 initial0 extract0) =
    Foldl.FoldM step initial0 extract0
  where
    step s x = f x >>= step0 s
