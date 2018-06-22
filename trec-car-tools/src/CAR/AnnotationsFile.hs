module CAR.AnnotationsFile where

import CAR.Types
import qualified CAR.TocFile as Toc

data AnnotationsFile = AnnotationsFile (Toc.IndexedCbor PageId Page)

openAnnotations :: FilePath -> IO AnnotationsFile
openAnnotations = fmap AnnotationsFile . Toc.open . Toc.IndexedCborPath

lookupPage :: PageId -> AnnotationsFile -> Maybe Page
lookupPage name (AnnotationsFile x) = Toc.lookup name x

pageIds :: AnnotationsFile -> [PageId]
pageIds (AnnotationsFile x) = Toc.keys x

pages :: AnnotationsFile -> [Page]
pages (AnnotationsFile x) = Toc.toList x
