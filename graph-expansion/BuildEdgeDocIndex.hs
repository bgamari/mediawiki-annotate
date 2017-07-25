import Control.Applicative
import Data.Foldable
import Data.Maybe
import Data.Monoid

import Options.Applicative
import EdgeDocCorpus
import EdgeDocIndex
import CAR.Types
import SimplIR.Term as Term
import SimplIR.DiskIndex as DiskIndex

modes :: Parser (IO ())
modes = subparser
    $ command "build" (info buildMode mempty)
   <> command "query" (info queryMode mempty)
  where
    buildMode =
        go <$> option str (long "output" <> short 'o' <> help "output index path")
           <*> argument str (help "articles file")
      where
        go outputPath articlesPath = do
            pages <- readCborList articlesPath
            _ <- indexEdgeDocs outputPath (pagesToEdgeDocs pages)
            return ()

    queryMode =
        go <$> option (OnDiskIndex <$> str) (long "index" <> short 'i' <> help "index path")
           <*> some (argument (Term.fromString <$> str) (help "query terms"))
      where
        go :: OnDiskIndex DocMeta Int -> [Term] -> IO ()
        go indexPath terms = do
            idx <- DiskIndex.openOnDiskIndex indexPath
            let postings = fold $ mapMaybe (`DiskIndex.lookupPostings` idx) terms
            print postings

main :: IO ()
main = do
    mode <- execParser $ info (helper <*> modes) mempty
    mode
