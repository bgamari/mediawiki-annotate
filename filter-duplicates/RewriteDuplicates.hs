{-# LANGUAGE RecordWildCards #-}

import Data.Maybe
import Data.Monoid
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.ByteString.Lazy as BSL
import Options.Applicative

import CAR.CarExports
import CAR.Types
import CAR.FilterDuplicates.ConnectedComponent
import CAR.FilterDuplicates.Utils

opts :: Parser (FilePath, FilePath, FilePath)
opts = (,,)
    <$> option str (short 'o' <> long "output" <> help "output pages file")
    <*> option str (short 'd' <> long "duplicates" <> help "duplicates file")
    <*> argument str (help "input pages file" <> metavar "PAGES")

main :: IO ()
main = do
    (outputFile, duplicatesFile, inputFile) <- execParser $ info (helper <*> opts) mempty

    edges <- parseDuplicates <$> readFile duplicatesFile
    let ccs = connectedComponents [ Edge a b | (a, b) <- edges ]

        rewriteIds :: HM.HashMap ParagraphId ParagraphId
        rewriteIds = HM.fromList
            [ (pid, canonical)
            | cc <- ccs
            , canonical : rest <- pure $ HS.toList cc
            , pid <- rest
            ]

    let canonicalParaIds :: HS.HashSet ParagraphId
        canonicalParaIds = HS.fromList $ HM.elems rewriteIds
    canonicalParagraphs <- fetchParagraphs inputFile canonicalParaIds

    let rewritePara :: Paragraph -> Paragraph
        rewritePara = \para -> fromMaybe para $ HM.lookup (paraId para) rewrites
          where
            rewrites :: HM.HashMap ParagraphId Paragraph
            rewrites = HM.fromList
                [ (pid, para)
                | (pid, canonicalParaId) <- HM.toList rewriteIds
                , Just para <- pure $ HM.lookup canonicalParaId canonicalParagraphs
                ]

    (prov, pages) <- readPagesFileWithProvenance inputFile
    writeCarFile outputFile prov $ map (rewritePage rewritePara) pages

rewritePage :: (Paragraph -> Paragraph) -> Page -> Page
rewritePage rewritePara page@(Page{..}) =
    page {pageSkeleton = map rewriteSkeleton pageSkeleton}
  where
    rewriteSkeleton (Section a b skels) = Section a b (map rewriteSkeleton skels)
    rewriteSkeleton (Para para)         = Para (rewritePara para)
    rewriteSkeleton (Image a skels)     = Image a (map rewriteSkeleton skels)
    rewriteSkeleton (List n para)       = List n (rewritePara para)

-- | Fetch the 'Paragraph's having the given 'ParagraphId's.
fetchParagraphs :: FilePath -> HS.HashSet ParagraphId
                -> IO (HM.HashMap ParagraphId Paragraph)
fetchParagraphs pagesFile interestingParas =
    HM.fromList
    . mapMaybe isInteresting
    . concatMap toParagraphs
    <$> readPagesFile pagesFile
  where
    isInteresting para
      | pid `HS.member` interestingParas = Just (pid, para)
      | otherwise                        = Nothing
      where pid = paraId para
