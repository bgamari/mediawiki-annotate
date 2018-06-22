{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

import Options.Applicative
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Aeson
import Data.Foldable
import Data.Semigroup
import Data.Tuple
import Data.Maybe
import qualified  Data.Map as M
import qualified Data.Text as T

import qualified  SimplIR.Ranking as R
import CAR.Types
import CAR.Types.Files

type CategoryName = PageName

newtype SectionScores = SectionScores {
                         sectionScores :: M.Map CategoryName [(SectionHeading, Double)]
                       }
    deriving (Semigroup, Monoid)

instance FromJSON SectionScores where
    parseJSON = withObject "section scores" $ \o -> do
        cat <- o .: "category"
        recs <- o .: "recs" >>= mapM parsePair
        return $ SectionScores $ M.singleton cat recs
      where
        parsePair o = (,) <$> fmap SectionHeading (o .: "_1")
                          <*> o .: "_2"


data Opts = Opts { sectionScoresFile :: FilePath
                 , inputOutlinesFile :: FilePath
                 , outputOutlinesfile :: FilePath
                 }

opts :: Parser Opts
opts =
    Opts
    <$> argument str (help "sectionScores json file" <> metavar "SectionScoresJson")
    <*> argument str (help "input pages file" <> metavar "InputPagesCbor")
    <*> argument str (help "output outline file" <> metavar "OutlineCbor")


main ::  IO ()
main = do
    Opts{..} <- execParser $ info (helper <*> opts) mempty

    Right sectionScoreEntries <- mapM eitherDecode . BSL.lines <$> BSL.readFile sectionScoresFile
       :: IO (Either String [SectionScores])

    let sectionScoresMap = sectionScores $ fold sectionScoreEntries
    --print $ M.toList $ sectionScoresMap
    (prov, outlines) <- readOutlinesFileWithProvenance inputOutlinesFile
       :: IO (Provenance, [Stub])

    let renameCat (PageName cat) =   -- TREC CAR cat -> epfl cat
            let cat' =
                  case T.stripPrefix "Category:" cat of
                      Just name -> T.map space2Underscore name
                      Nothing -> cat
--                 cat' =  T.map space2Underscore $ fromJust  $ T.stripPrefix "Category:" cat
            in PageName cat'
          where space2Underscore ' ' = '_'
                space2Underscore x = x

    let recommendSections :: Stub -> [SectionHeading] -- Ranking Double SectionHeading
        recommendSections outline =
           let sectionRanking =
                   R.toSortedList            -- [heading]
                   $ R.takeTop 10
                   $ R.fromList
                   $ map swap                -- [(score, heading)]
                   $ M.toList
                   $ M.fromListWith (+)      -- now we have (heading -> aggregated score)
                   $ [ (sectionHeading, weight)
                    | Just cats <- pure $ getMetadata _CategoryNames $ stubMetadata outline
                    , cat <- cats
                    , Just sections <- pure $ (renameCat cat ) `M.lookup` sectionScoresMap
                    , (sectionHeading, weight) <- sections
                    ]
           in [ heading
              | (score, heading) <- sectionRanking]

    let toStub :: [SectionHeading] -> Stub -> Stub
        toStub headings stub =
            stub { stubSkeleton =  skel }
          where skel = [ Section sh (sectionHeadingToId sh) []
                       | sh <- headings
                       ]


    let newOutlines :: [Stub]
        newOutlines = fmap (\stub -> toStub (recommendSections stub) stub) $ outlines


    writeCarFile outputOutlinesfile prov newOutlines


    print newOutlines
