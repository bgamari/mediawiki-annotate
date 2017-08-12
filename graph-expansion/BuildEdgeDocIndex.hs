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
import SimplIR.Term as Term
import SimplIR.SimpleIndex as Index

edgeDocModes :: Parser (IO ())
edgeDocModes = subparser
    $ command "index" (info buildMode mempty)
   <> command "query" (info queryMode mempty)
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
           <*> some (argument (Term.fromString <$> str) (help "query terms"))
      where
        go :: Index.OnDiskIndex Term EdgeDoc Int -> [Term] -> IO ()
        go indexPath terms = do
            idx <- Index.open indexPath
            let postings = fold $ map (Index.lookupPostings idx) terms
            print postings

entityModes :: Parser (IO ())
entityModes = subparser
    $ command "index" (info buildMode mempty)
   <> command "query" (info queryMode mempty)
  where
    buildMode =
        go <$> option str (long "output" <> short 'o' <> help "output index path")
           <*> argument str (metavar "CBOR" <> help "kb articles file")
      where
        go outputPath articlesPath = do
            pages1 <- readCborList articlesPath
            let !inlinkInfo = collectInlinkInfo pages1
            pages2 <- readCborList articlesPath

            let docTerms :: KbDoc -> [Term]
                docTerms doc =
                       foldMap tokeniseText (kbDocLeadText doc)
                    ++ foldMap tokeniseText (HM.keys $ anchorCount inlinks)
                    ++ foldMap tokenisePageId (HM.keys $ disambiguationCount inlinks)
                  where
                    inlinks = fromMaybe mempty
                              $ HM.lookup (kbDocCanonicalName doc) (documentInlinks inlinkInfo)
                    tokenisePageId = textToTokens' . getPageName
                    tokeniseText   = textToTokens'

            Index.buildTermFreq outputPath
                [ (kbDocPageId doc, docTerms doc)
                | doc <- map pageToKbDoc pages2
                ]
            return ()

    queryMode =
        go <$> option (OnDiskIndex <$> str) (long "index" <> short 'i' <> help "index path")
           <*> some (argument (Term.fromString <$> str) (help "query terms"))
      where
        go :: Index.OnDiskIndex Term PageId Int -> [Term] -> IO ()
        go indexPath terms = do
            idx <- Index.open indexPath
            let postings = fold $ map (Index.lookupPostings idx) terms
            print postings

modes = subparser
    $ command "entity"  (info entityModes mempty)
   <> command "edgedoc" (info entityModes mempty)

main :: IO ()
main = do
    mode <- execParser $ info (helper <*> modes) mempty
    mode
