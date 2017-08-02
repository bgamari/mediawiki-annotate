{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid
import Data.List

import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.IO as TL
import qualified Data.HashMap.Strict as HM

import Options.Applicative

import CAR.Types
import CAR.Utils
import CAR.AnnotationsFile as AnnsFile
                                      
options :: Parser (FilePath, FilePath, Mode)
options =
    (,,)
      <$> argument str (help "annotations file" <> metavar "FILE" <> help "unprocessed all.cbor file")
      <*> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")
      <*> mode
  where
    mode = flag' LegalPageIds (long "legal-page-ids" <> help "Write list of legal page IDs")
       <|> flag' Redirects (long "redirects" <> help "Write list of redirects")


data Mode = LegalPageIds | Redirects

main :: IO ()
main = do
    (unprocessedPagesFile, outputFile, mode) <- execParser $ info (helper <*> options) mempty
    unprocessedPages <- openAnnotations unprocessedPagesFile

    let entityRedirects = entityRedirectMap  $ AnnsFile.pages unprocessedPages
        resolveRedirect = resolveRedirectFun entityRedirects
        redirectedPageIds =  HM.keys entityRedirects
        legalPageIds = filter (\pageId -> pageId == resolveRedirect pageId)
                     $ HM.elems entityRedirects

    case mode of
      LegalPageIds ->
        let formatPageIdToName :: PageId -> TB.Builder
            formatPageIdToName pageId =
                (TB.fromString $ unpackPageId pageId)
                <> "\t"
                <> (TB.fromString $ unpackPageName finalPageName)
              where finalPageName = pageIdToName pageId
        in TL.writeFile outputFile $ TB.toLazyText
                $ mconcat
                $ intersperse "\n"
                $ fmap formatPageIdToName
                legalPageIds

      Redirects ->
        let formatPageIdToRedirect :: PageId -> TB.Builder
            formatPageIdToRedirect pageId =
                (TB.fromString $ unpackPageId pageId)
                <> "\t"
                <> (TB.fromString $ unpackPageId finalPageId)
              where finalPageId = resolveRedirect pageId

        in TL.writeFile outputFile $ TB.toLazyText
                $ mconcat
                $ intersperse "\n"
                $ fmap formatPageIdToRedirect
                redirectedPageIds
