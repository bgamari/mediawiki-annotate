{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ApplicativeDo #-}

module CAR.FillMetadata  where


import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Semigroup hiding (option)
import Data.Foldable (foldl')

import CAR.Utils
import CAR.Types
import CAR.Utils.Redirects

fixLinks:: (PageId -> PageId) -> Page -> Page
fixLinks redirectResolver page =
    page {pageSkeleton = fmap goSkeleton (pageSkeleton  page)}
      where
        goSkeleton (Section x y children) = Section x y (fmap goSkeleton children)
        goSkeleton (Para p) = Para (goParagraph p)
        goSkeleton (Image x skel) = Image x (fmap goSkeleton skel)
        goSkeleton (List x p) = List x (goParagraph p)

        goParagraph (Paragraph x bodies) = Paragraph x (fmap goParaBody bodies)

        goParaBody (ParaText t) = ParaText t
        goParaBody (ParaLink l) = ParaLink l {linkTarget = pageIdToName  newLinkTargetId
                                             , linkTargetId = newLinkTargetId}
            where newLinkTargetId = redirectResolver (linkTargetId l)


-- Assume that category page ids are already filled in from the inport stage;  otherwise call buildCategoryMap
buildRedirectMap :: Page -> HM.HashMap PageId Acc
buildRedirectMap page =
    foldl' (HM.unionWith (<>)) mempty (redirect)
  where
    redirect =
        [ HM.singleton pid
          $ mempty { accRedirectNames = HS.singleton (pageName page) }
        | RedirectPage pid <- pure (pagemetaType $ pageMetadata page)
        ]

-- Assume that category page ids are already filled in from the inport stage;  otherwise call buildCategoryMap
-- Also assume that redirect are already resolved
buildMap :: Page -> HM.HashMap PageId Acc
buildMap page =
    foldl' (HM.unionWith (<>)) mempty (disambigs <> inlinks)
  where
    disambigs =
        [ HM.singleton (linkTargetId link)
          $ mempty { accDisambigNames = HS.singleton (pageName page)
                   , accDisambigIds = HS.singleton (pageId page) }
        | DisambiguationPage <- pure (pagemetaType $ pageMetadata page)
        , link <- pageLinks page
        ]
    inlinks =
        [ HM.singleton (linkTargetId link)
          $ mempty { accInlinkIds = HS.singleton (pageId page) }
        | pageIsArticle page || pageIsCategory page
        , link <- pageLinks page
        ]


extractAllCategoryIds :: FilePath -> IO (HS.HashSet PageId)
extractAllCategoryIds inputPath = do
    HS.fromList . map pageId . filter pageIsCategory <$> readPagesFile inputPath
 

buildCategoryMap :: HS.HashSet PageId -- ^ set of known categories
         -> Page -> HM.HashMap PageId Acc
buildCategoryMap allCategoryIds page =
    foldl' (HM.unionWith (<>)) mempty [categories]
  where
    categories =
        HM.singleton (pageId page)
        $ mempty { accCategoryNames = HS.fromList $ map linkTarget categoryLinks
                 , accCategoryIds = HS.fromList $ map linkTargetId categoryLinks
                 }
      where
        categoryLinks =
            [ link
            | link <- pageLinks page
            , linkTargetId link `HS.member` allCategoryIds
            ]
    

data Acc = Acc { accRedirectNames :: !(HS.HashSet PageName)
               , accDisambigNames :: !(HS.HashSet PageName)
               , accDisambigIds   :: !(HS.HashSet PageId)
               , accCategoryNames :: !(HS.HashSet PageName)
               , accCategoryIds   :: !(HS.HashSet PageId)
               , accInlinkIds     :: !(HS.HashSet PageId)
               }

instance Monoid Acc where
    mempty = Acc m m m m m m
      where
        m :: Monoid m => m
        m = mempty
    mappend = (<>)

instance Semigroup Acc where
    Acc a1 b1 c1 d1 e1 f1 <> Acc a2 b2 c2 d2 e2 f2 =
        Acc (a1<>a2) (b1<>b2) (c1<>c2) (d1<>d2) (e1<>e2) (f1<>f2)

fillMetadata :: HM.HashMap PageId Acc -> Page -> Page
fillMetadata acc page =
    page { pageMetadata = (pageMetadata page)
                          { pagemetaRedirectNames       = HS.toList . accRedirectNames <$> things
                          }
         }
  where
    things = HM.lookup (pageId page) acc



fillRedirectMetadata :: HM.HashMap PageId Acc -> Page -> Page
fillRedirectMetadata acc page =
    page { pageMetadata = (pageMetadata page)
                          { pagemetaDisambiguationNames = HS.toList . accDisambigNames <$> things
                          , pagemetaDisambiguationIds   = HS.toList . accDisambigIds <$> things
                          , pagemetaInlinkIds           = HS.toList . accInlinkIds <$> things
                          }
         }
  where
    things = HM.lookup (pageId page) acc


fillCategoryMetadata :: HM.HashMap PageId Acc -> Page -> Page
fillCategoryMetadata acc page =
    page { pageMetadata = (pageMetadata page)
                          { pagemetaCategoryNames       = HS.toList . accCategoryNames <$> things
                          , pagemetaCategoryIds         = HS.toList . accCategoryIds <$> things
                          }
         }
  where
    things = HM.lookup (pageId page) acc