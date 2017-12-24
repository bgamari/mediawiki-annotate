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

opts :: Parser (FilePath, FilePath, Maybe FilePath)
opts = (,,)
    <$> option str (short 'o' <> long "output" <> help "output rewrite table file")
    <*> option str (short 'd' <> long "duplicates" <> help "duplicates file")
    <*> optional (option str (long "table" <> help "deduplication table for preserving choices of canonical page ids"))

main :: IO ()
main = do
    (outputFile, duplicatesFile, prevTablePathOpt) <- execParser $ info (helper <*> opts) mempty

    edges <- parseDuplicates <$> readFile duplicatesFile
    let ccs ::  [HS.HashSet ParagraphId]
        ccs = connectedComponents [ Edge a b | (a, b) <- edges ]

    prevTable <- traverse (fmap (HM.fromList . parseDuplicates) . readFile) prevTablePathOpt
            :: IO (Maybe (HM.HashMap ParagraphId ParagraphId))

    let rewriteIds :: [(ParagraphId, ParagraphId)]
        rewriteIds =
            [ (pid, canonical)
            | cc <- ccs
--             , canonical : rest <- pure $ HS.toList cc
            , (canonical, rest)  <- pure $ selectCanonicalPid prevTable $  HS.toList cc
            , pid <- rest
            ]


    writeFile outputFile $ unlines
      [ unpackParagraphId pid <> "\t" <> unpackParagraphId canonical
      | (pid, canonical) <- rewriteIds
      ]

selectCanonicalPid :: Maybe (HM.HashMap ParagraphId ParagraphId) -> [ParagraphId] -> (ParagraphId, [ParagraphId])
selectCanonicalPid Nothing (canonical : rest)  =
   (canonical, rest)
selectCanonicalPid (Just table) cc@(canonical:rest) =
   case canonical `HM.lookup` table of
       Just canonicalOld -> -- case 1, 2,  and case 3 by arbitrary choice
           let rest' = filter (/= canonicalOld)  $ cc
           in (canonicalOld, rest')
       Nothing ->  -- case 4, case 5
           (canonical, rest)

   -- table row = old cluster as list of pid elements and it's canonical pid


   -- Case 1
   -- clusters are the same, but need to choose the right canonical:
   -- find table row that matches `cc`   (table row that contains all elems of cc)
   -- we approximate `all` by choosing the row with at least one match  -- otherwise this is case 3
   -- use that table row's canonical pid

   -- Case 2
   -- two clusters could be merged in one table row:
   -- both clusters get the same canonical pid - problem solved!

   -- Case 3
   -- two table rows could be merged into one cluster (because missing links were discovered):
   -- chose one of the two table rows , use canonical pid of that row
   -- (consistently assign to all elems of cluster to this canonical pid)

   -- Case 4
   -- new cluster, does not have a table row:
   -- chose canonical pid randomly

   -- Case 5
   -- table row not represented in this data set:
   -- ignore table row

