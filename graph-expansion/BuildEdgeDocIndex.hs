import Control.Applicative
import Data.Foldable
import Data.Maybe
import Data.Monoid

import Options.Applicative
import EdgeDocCorpus
import CAR.Types
import CAR.Retrieve
import SimplIR.Term as Term
import SimplIR.SimpleIndex as Index

modes :: Parser (IO ())
modes = subparser
    $ command "build" (info buildMode mempty)
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

main :: IO ()
main = do
    mode <- execParser $ info (helper <*> modes) mempty
    mode
