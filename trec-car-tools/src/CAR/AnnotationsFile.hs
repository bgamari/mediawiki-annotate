module CAR.AnnotationsFile (
      PageBundle(..)
    , bundleAllPages, bundleLookupAllPageNames
    , openPageBundle
    ) where

import CAR.Types
import qualified CAR.TocFile as Toc
import CAR.NameToIdMap
import qualified Data.Set as S
import Data.Maybe (fromMaybe)

data AnnotationsFile = AnnotationsFile (Toc.IndexedCbor PageId Page)

openAnnotations :: FilePath -> IO AnnotationsFile
openAnnotations = fmap AnnotationsFile . Toc.open . Toc.IndexedCborPath

lookupPage :: PageId -> AnnotationsFile -> Maybe Page
lookupPage name (AnnotationsFile x) = Toc.lookup name x

pageIds :: AnnotationsFile -> [PageId]
pageIds (AnnotationsFile x) = Toc.keys x

pages :: AnnotationsFile -> [Page]
pages (AnnotationsFile x) = Toc.toList x

-------------------

data AnnotationsFileStub = AnnotationsFileStub (Toc.IndexedCbor PageId Stub)

openStubAnnotations :: FilePath -> IO AnnotationsFileStub
openStubAnnotations  = fmap AnnotationsFileStub . Toc.open . Toc.IndexedCborPath

lookupStub:: PageId -> AnnotationsFileStub -> Maybe Stub
lookupStub name (AnnotationsFileStub x) = Toc.lookup name x

lookupStubAsPage :: PageId -> AnnotationsFileStub -> Maybe Page
lookupStubAsPage name ann =  stubToPage <$> lookupStub name ann

---------

openEitherAnnotations :: FilePath -> IO EitherAnnotationsFile
openEitherAnnotations cborPath = do
    (_, cborContents) <- readCarPagesOrOutlineWithProvenance cborPath
    case cborContents  of
        Left _ ->  PagesAnnotations <$> openAnnotations cborPath
        Right _ -> StubsAnnotations <$> openStubAnnotations cborPath

data EitherAnnotationsFile = PagesAnnotations AnnotationsFile
                           | StubsAnnotations AnnotationsFileStub

lookupEither:: PageId -> EitherAnnotationsFile -> Maybe Page
lookupEither name ann =
    case ann of
        PagesAnnotations ann' -> lookupPage name ann'
        StubsAnnotations ann' -> lookupStubAsPage name ann'

toPageListEither :: EitherAnnotationsFile -> [Page]
toPageListEither ann =
    case ann of
        PagesAnnotations (AnnotationsFile toc) -> Toc.toList toc
        StubsAnnotations (AnnotationsFileStub toc) -> stubToPage <$> Toc.toList toc



------

data PageBundle = PageBundle { bundleProvenance :: Provenance
                             , bundleLookupPage :: PageId -> Maybe Page
                             , bundleLookupPageName :: PageName -> Maybe (S.Set PageId)
                             , bundleToc :: EitherAnnotationsFile
                             , bundleNameLookup ::  NameToIdMap
                             , bundleCborPath :: FilePath
                             }


openPageBundle :: FilePath -> IO PageBundle
openPageBundle cborPath = do
    toc <- openEitherAnnotations cborPath
    nameLookup <- openNameToIdMap cborPath
    (prov, _) <- readPagesOrOutlinesAsPagesWithProvenance cborPath
    return PageBundle {
                 bundleCborPath = cborPath
               , bundleProvenance = prov
               , bundleToc = toc
               , bundleNameLookup = nameLookup
               , bundleLookupPage = (`lookupEither` toc)
               , bundleLookupPageName = (nameLookup `pageNameToIdMaybeSet`)
               }
{-# NOINLINE openPageBundle #-}

bundleAllPages :: PageBundle -> [Page]
bundleAllPages bundle = toPageListEither (bundleToc bundle)

bundleLookupAllPageNames :: Foldable f => PageBundle -> f PageName -> S.Set PageId
bundleLookupAllPageNames bundle =
    foldMap (fromMaybe S.empty . bundleLookupPageName bundle)


