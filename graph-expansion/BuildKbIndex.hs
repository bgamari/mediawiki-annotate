{-# LANGUAGE BangPatterns #-}


import Data.List
import Data.Ord
import Control.Applicative
import Data.Foldable
import Data.Maybe
import Data.Monoid
import qualified Data.HashMap.Strict as HM

import Options.Applicative
import EdgeDocCorpus
import CAR.Types
import CAR.Retrieve
import CAR.KnowledgeBase
import CAR.Utils
import SimplIR.Term as Term
import SimplIR.SimpleIndex as Index

import qualified Data.Text.Lazy as TL

data TextPart = FullText | LeadText
data LinkMode = NoLinks | WithLinks



edgeDocModes :: Parser (IO ())
edgeDocModes = subparser
    $ command "index" (info (helper <*> buildMode) mempty)
   <> command "query" (info (helper <*> queryMode) mempty)
  where
    buildMode =
        go <$> option str (long "output" <> short 'o' <> help "output index path")
           <*> argument str (metavar "CBOR" <> help "kb articles file")
      where
        go outputPath articlesPath = do
            pages <- readCborList articlesPath
            Index.buildTermFreq outputPath
                [ (edoc, textToTokens' $ edgeDocContent edoc)
                | edoc <- pagesToEdgeDocs pages
                ]
            return ()

    queryMode =
        go <$> option (OnDiskIndex <$> str) (long "index" <> short 'i' <> help "index path")
           <*> some (argument (Term.fromString <$> str) (metavar "TERM" <> help "query terms"))
      where
        go :: Index.OnDiskIndex Term EdgeDoc Int -> [Term] -> IO ()
        go indexPath terms = do
            idx <- Index.open indexPath
            let postings = fold $ map (Index.lookupPostings idx) terms
            print postings

entityModes :: Parser (IO ())
entityModes = subparser
    $ command "index" (info (helper <*> indexMode) mempty)
   <> command "query" (info (helper <*> queryMode) mempty)
  where
    indexMode =
        go <$> option str (long "output" <> short 'o' <> help "output index path")
           <*> argument str (metavar "CBOR" <> help "kb articles file")
           <*> flag FullText LeadText (long "lead" <> help "Index only lead text (if not set, index full text)")
      where
        go outputPath articlesPath textPart = do
            !resolveRedirect <- resolveRedirectFactory <$> readCborList articlesPath

            !inlinkInfo <- collectInlinkInfo resolveRedirect <$> readCborList articlesPath
            pages2 <- readCborList articlesPath

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

    queryMode =
        go <$> option (OnDiskIndex <$> str) (long "index" <> short 'i' <> help "index path")
           <*> some (argument (Term.fromString <$> str) (metavar "TERM" <> help "query terms"))
      where
        go :: Index.OnDiskIndex Term PageId Int -> [Term] -> IO ()
        go indexPath terms = do
            idx <- Index.open indexPath
            let postings = fold $ map (Index.lookupPostings idx) terms
            mapM_ print $ sortBy (flip $ comparing snd) postings

paragraphModes :: Parser (IO ())
paragraphModes = subparser
    $ command "index" (info (helper <*> indexMode) mempty)
   <> command "query" (info (helper <*> queryMode) mempty)
  where
    indexMode =
        go <$> option str (long "output" <> short 'o' <> help "output index path")
           <*> argument str (metavar "CBOR" <> help "paragraph file")
           <*> flag NoLinks WithLinks (long "links" <> help "Index also link targets ")
      where
        go outputPath paragraphsPath textPart = do

            paragraphs <- readCborList paragraphsPath

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

    queryMode =
        go <$> option (OnDiskIndex <$> str) (long "index" <> short 'i' <> help "index path")
           <*> some (argument (Term.fromString <$> str) (metavar "TERM" <> help "query terms"))
      where
        go :: Index.OnDiskIndex Term ParagraphId Int -> [Term] -> IO ()
        go indexPath terms = do
            idx <- Index.open indexPath
            let postings = fold $ map (Index.lookupPostings idx) terms
            mapM_ print $ sortBy (flip $ comparing snd) postings

modes = subparser
    $ command "entity"  (info (helper <*> entityModes) mempty)
   <> command "edgedoc" (info (helper <*> edgeDocModes) mempty)
   <> command "paragraph" (info (helper <*> paragraphModes) mempty)

main :: IO ()
main = do
    mode <- execParser $ info (helper <*> modes) mempty
    mode
