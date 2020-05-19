{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}


import Options.Applicative
import CAR.Types
import CAR.Utils


import System.Random
import System.Random.Shuffle
import Control.Monad.Random

import qualified Data.Text as T
import Data.String
import Data.List

data CrateMode = CrateRelevance | CrateCluster | CrateTransition
    deriving (Show, Read, Ord, Eq, Enum, Bounded)

args :: Parser (FilePath, FilePath, CrateMode, Int)
args =
    (,,,)
      <$> argument str (metavar "CBOR" <> help "Cbor Pages File")
      <*> argument str (metavar "FILE" <> help "Output file")
      <*> argument auto (metavar "MODE" <> help ("convertion mode, one of " ++ (show [minBound @CrateMode .. maxBound])))
      <*> option auto (short 'k' <> long "num-pages" <> metavar "K" <> help "number of pages to convert")

data BertData = BertData { targetLabel :: Int
                         , id1 :: T.Text
                         , id2 :: T.Text
                         , text1 :: T.Text
                         , text2 :: T.Text
                         }
    deriving (Show, Eq)

writeBert :: FilePath -> [BertData] -> IO ()
writeBert outputFile bertData =
    writeFile outputFile $ T.unpack $ T.unlines
      [ T.intercalate "\t" [(T.pack $ show targetLabel), id1, id2, text1, text2]
      | BertData{..} <- bertData
      ]

main :: IO ()
main = do
    (pagesCborFile, outFile, crateMode, numPages) <- execParser $ info (helper <*> args) mempty
    pages <- readPagesFile pagesCborFile

    let cratifyPage = case crateMode of
            CrateRelevance -> relevanceData
            CrateCluster -> clusterData
            CrateTransition  -> transitionData
            x -> const $ fail ("Not implemented: "++  (show x))
    outData <-  mapM cratifyPage $ take numPages pages
    writeBert outFile $ concat outData

relevanceData :: Page -> IO [BertData]
relevanceData page = do
     let secs :: [(SectionPath, [SectionHeading], [PageSkeleton])]
         secs = pageSections page
         posData =
            [ BertData 1 (sectionPathToId sectionPath) (paragraphToId p) (headingsToText headings) (paragraphToText p)
            | (sectionPath, headings, skels) <- secs
            , Para p@(Paragraph {})  <- skels
            ]

         negData =
            [ BertData 0 (sectionPathToId sectionPath) (paragraphToId p) (headingsToText headings2) (paragraphToText p)
            | s1@(sectionPath, _headings, skels) <- secs
            , s2@(sectionPath2, headings2, _skels2) <- secs
            , sectionPath /= sectionPath2
            , Para p@(Paragraph {})  <- skels
            ]

     balanceData posData negData


clusterData :: Page -> IO [BertData]
clusterData page = do
     let secs :: [(SectionPath, [SectionHeading], [PageSkeleton])]
         secs = pageSections page

         bothData =
            [ BertData target (paragraphToId p1) (paragraphToId p2) (paragraphToText p1) (paragraphToText p2)
            | s1@(sectionPath1, headings1, skels1) <- secs
            , s2@(sectionPath2, headings2, skels2) <- secs
            , let target = if (sectionPathToId sectionPath1) /= (sectionPathToId sectionPath2) then 0 else 1
            , Para p1@(Paragraph {paraId=paraId1})  <- skels1
            , Para p2@(Paragraph {paraId=paraId2})  <- skels2
            , paraId1 /= paraId2
            , paraId1 < paraId2
            ]

--         (posData, negData) = partition (\BertData {targetLabel = l} -> l == 1) bothData
--     putStrLn $ show (length posData) <> "  " <> show (length negData)
--     balanceData posData negData
     return bothData


transitionData :: Page -> IO [BertData]
transitionData page = do
     let posData =
            [ BertData 1 (paragraphToId p1) (paragraphToId p2) (paragraphToText p1) (paragraphToText p2)
            | [p1@(Paragraph {paraId=paraId1}), p2@(Paragraph {paraId=paraId2})] <- slidingWindow 2 $ pageParas page
            ]

         negData =
            [ BertData 0 (paragraphToId p1) (paragraphToId p2) (paragraphToText p1) (paragraphToText p2)
            | [p1@(Paragraph {paraId=paraId1}), _, _, _, p2@(Paragraph {paraId=paraId2})] <- slidingWindow 5 $ pageParas page
            ]

     putStrLn $ show (length posData) <> "  " <> show (length negData)
     balanceData posData negData
--     return bothData



balanceData :: [BertData] -> [BertData] -> IO [BertData]
balanceData posData negData = do
    let totals = min (length posData) (length negData)
    posData' <- shuffleList posData
              :: IO [BertData]
    negData' <- shuffleList negData
              :: IO [BertData]
    let result :: [BertData]
        result = (take totals posData') <> (take totals negData')

    return result


--shuffleList :: Traversable t
--                => t a
--                -> IO (t a)
--shuffleList = evalRandIO . traverse shuffleM
shuffleList :: [a] -> IO [a]
shuffleList lst = do
    gen <- newStdGen
    return $ shuffle' lst (length lst) gen

headingsToText :: [SectionHeading] -> T.Text
headingsToText headings =
    T.unwords [ getSectionHeading heading | heading <- headings]

sectionPathToId :: SectionPath -> T.Text
sectionPathToId sectionPath =
    T.pack $ escapeSectionPath sectionPath

paragraphToId :: Paragraph -> T.Text
paragraphToId p@(Paragraph {paraId=paraId} ) =
    T.pack $ unpackParagraphId paraId

paragraphToText :: Paragraph -> T.Text
paragraphToText (Paragraph  _ bodies) =
    T.concat $ fmap toText bodies
  where toText (ParaText text) = text
        toText (ParaLink link) = linkAnchor link

slidingWindow :: Int -> [a] -> [[a]]
slidingWindow chunkSize lst =
    go lst
  where go :: [a] -> [[a]]
        go lst' | length lst' <= chunkSize = [lst']
        go lst' = (take chunkSize lst': go (drop 1 lst'))