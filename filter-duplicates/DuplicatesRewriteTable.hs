import Data.Maybe
import Data.Monoid
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.ByteString.Lazy as BSL
import Options.Applicative

import CAR.CarExports
import CAR.Types
import ConnectedComponent
import Utils

opts :: Parser (FilePath, FilePath)
opts = (,)
    <$> option str (short 'o' <> long "output" <> help "output rewrite table file")
    <*> option str (short 'd' <> long "duplicates" <> help "duplicates file")

main :: IO ()
main = do
    (outputFile, duplicatesFile) <- execParser $ info (helper <*> opts) mempty

    edges <- parseDuplicates <$> readFile duplicatesFile
    let ccs = connectedComponents [ Edge a b | (a, b) <- edges ]

    let rewriteIds :: [(ParagraphId, ParagraphId)]
        rewriteIds = 
            [ (pid, canonical)
            | cc <- ccs
            , canonical : rest <- pure $ HS.toList cc
            , pid <- rest
            ]

    writeFile outputFile $ unlines 
      [ unpackParagraphId pid <> "\t" <> unpackParagraphId canonical
      | (pid, canonical) <- rewriteIds
      ]
    