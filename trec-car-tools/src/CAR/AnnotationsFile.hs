module CAR.AnnotationsFile where

import CAR.Types
import qualified CAR.TocFile as Toc
import CAR.Types.Files

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
    (prov, cborContents) <- readCarPagesOrOutlineWithProvenance cborPath
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


