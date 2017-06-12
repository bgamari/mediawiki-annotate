{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

import Data.Monoid hiding (All, Any)
import System.IO

import Options.Applicative
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe

import CAR.Types
import SimplIR.Galago as Galago

import CAR.KnowledgeBase


opts :: Parser (FilePath, FilePath)
opts =
    (,)
    <$> argument str (help "input file" <> metavar "PARAGRAPHS")
    <*> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output warc file")


wikiAnchorStopwords = [ "more....", "wikipedia article", "source: wikipedia", "here", "wiki", "wikipedia"]
wikiAnchorStopphrases = [ "wikipedia the free encyclopedia", "en.wikipedia.org", "full article at wikipedia.org"
                        , "wikipedia the free encyclopedia", "wikipedia, the free encyclopedia"
                        , "http://en.wikipedia.org/wiki/", "wikipedia"]


skeletonToXml :: [PageSkeleton] -> TB.Builder
skeletonToXml list =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?><articles loadtime=\"0 sec\" rendertime=\"0.06 sec\" totaltime=\"0.06 sec\"><article> "
    <> foldMap skelToXml list
    <>" </article></articles>"
  where

    skelToXml :: PageSkeleton -> TB.Builder
    skelToXml (Para (Paragraph paraId parabodies ) )  =
        " <paragraph><sentence> "  -- id = 1
        <> foldMap bodyToXml parabodies
        <> " </sentence></paragraph> "
    skelToXml (Section (SectionHeading heading) _ parabodies )  =
        " <heading level=\"1\"> "<> TB.fromText (T.replace "\n" " " heading) <>" </heading> "  -- id = 1
    skelToXml (Image {} )  = mempty

bodyToXml :: ParaBody -> TB.Builder
bodyToXml (ParaText text) = " " <> TB.fromText (T.replace "\n" " " text) <> " "
bodyToXml (ParaLink (Link {..})) =
    " <link>"<>
    " <anchor> "
    <>TB.fromText (T.replace "\n" "  " linkAnchor)
    <>" </anchor> "
    <>" <target> "
    <>TB.fromString (unpackPageId $ linkTargetId)
    <>" </target> "
    <> maybe "" renderLink linkSection
    <>" </link> "
  where renderLink linkSection' = " <linksection> "
                              <>TB.fromText linkSection'
                              <>" </linksection> "



contentParagraph :: Paragraph -> String
contentParagraph (Paragraph paraId bodies) =
    concatMap go bodies ++ "\n"
  where
    go (ParaText t) = T.unpack t
    go (ParaLink l) = T.unpack $ linkAnchor l

toGalagoDoc :: Paragraph -> Galago.Document
toGalagoDoc para@(Paragraph paraId paraBody) =
    let xml = " <paragraph>" <> foldMap bodyToXml paraBody <> "</paragraph>"
        meta :: M.Map MetadataFieldName TL.Text
        meta = M.fromList [
                          ("paragraphId", TL.pack $ unpackParagraphId paraId)
                          , ("xml", TB.toLazyText xml)
                          , ("lastModified", "2000-01-01 00:00:00")
                          ]
        content = T.pack $ contentParagraph para

        phrase x = (TL.fromStrict x, Galago.DoNotTokenize)
        naturalText x = (TL.fromStrict x, Galago.DoTokenize)
    in Galago.Document { docId       = Galago.DocumentId $ T.pack $ unpackParagraphId paraId
                       , docMetadata = fmap TL.encodeUtf8 meta
                       , docFields   = M.fromList
                          [ ("content",              [naturalText content])
                          , ("content-exact",        [phrase content])
                          ]
                       }

main :: IO ()
main = do
    (inputFile, outputFile) <- execParser $ info (helper <*> opts) mempty
    paragraphs <- decodeCborList <$> BSL.readFile inputFile
    withFile outputFile WriteMode $ \h ->
        BSL.hPutStr h $ Galago.toWarc
            $ map toGalagoDoc
            paragraphs

