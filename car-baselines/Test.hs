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
    buildIndex <$> argument str (help "annotations file")

modeMerge :: Parser (IO ())
modeMerge =
    go
      <$> option str (long "output" <> short 'o' <> help "Output path")
      <*> many (argument (DiskIdx.OnDiskIndex <$> str) (help "annotations file"))
  where
    go :: FilePath -> [CarDiskIndex] -> IO ()
    go outPath parts = mapM DiskIdx.openOnDiskIndex parts >>= DiskIdx.merge outPath

modes :: Parser (IO ())
modes = subparser
    $  command "index" (info modeIndex fullDesc)
    <> command "merge" (info modeMerge fullDesc)

main :: IO ()
main = do
    mode <- execParser $ info (helper <*> modes) fullDesc
    mode

query :: CarDiskIndex -> IO ()
query diskIdx = do
    idx <- DiskIdx.openOnDiskIndex diskIdx
    let query = ["hello", "world"]
    return ()

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

buildIndex :: FilePath -> IO ()
buildIndex annotationsFile = do
    anns <- openAnnotations annotationsFile
    let paras = concatMap toParagraphs (pages anns)
    Foldl.foldM (DiskIdx.buildIndex 100000 "paragraphs.index")
              $ fmap (\p -> let terms = paragraphTerms p
                            in ((paraId p, DocLength $ sum terms), terms)
                     ) paras

    return ()
