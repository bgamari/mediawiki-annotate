{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad.IO.Class
import Data.Profunctor
import Data.Bifunctor
import Data.Maybe (fromMaybe)
import Data.Monoid

import CAR.Types
import CAR.AnnotationsFile
import CAR.CarExports
import SimplIR.RetrievalModels.QueryLikelihood
import SimplIR.TopK
import SimplIR.Types
import SimplIR.Term as Term
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

paragraphTerms :: Paragraph -> M.Map Term Int
paragraphTerms p =
    M.unionsWith (+)
    $ map (`M.singleton` 1)
    $ foldMap paraBodyTerms
    $ paraBody p
  where
    paraBodyTerms (ParaText t) = map (Term.fromText . T.toCaseFold) $ T.words t
    paraBodyTerms (ParaLink _ t) = map (Term.fromText . T.toCaseFold) $ T.words t

type CarDiskIndex = DiskIdx.OnDiskIndex (ParagraphId, DocumentLength) Int

modeIndex :: Parser (IO ())
modeIndex =
    go <$> option str (long "output" <> short 'o' <> help "Output index path")
       <*> argument str (help "annotations file")
  where
    go outFile annotationsFile = do
        anns <- openAnnotations annotationsFile
        let paras = concatMap toParagraphs (pages anns)
        Foldl.foldM (DiskIdx.buildIndex 100000 outFile)
                  $ fmap (\p -> let terms = paragraphTerms p
                                in ((paraId p, DocLength $ sum terms), terms)
                        ) paras

        return ()

modeMerge :: Parser (IO ())
modeMerge =
    go
      <$> option str (long "output" <> short 'o' <> help "Output path")
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
    $  command "index" (info modeIndex fullDesc)
    <> command "merge" (info modeMerge fullDesc)
    <> command "query" (info modeQuery fullDesc)

main :: IO ()
main = do
    mode <- execParser $ info (helper <*> modes) fullDesc
    mode

termPostings :: (Monad m, Ord p, Binary p)
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
