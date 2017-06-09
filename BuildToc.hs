{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

import Data.Monoid
import Options.Applicative
import Control.Monad

import CAR.Types
import CAR.TocFile as Toc

mode :: Parser (IO ())
mode = subparser
    $  command "pages" (info (helper <*> indexPages) fullDesc)
    <> command "paragraphs" (info (helper <*> indexParagraphs) fullDesc)
  where
    indexPages =
        void . Toc.createIndex pageId <$> argument str (help "articles cbor file" <> metavar "FILE")
    indexParagraphs =
        void . Toc.createIndex paraId <$> argument str (help "paragraphs cbor file" <> metavar "FILE")


main :: IO ()
main = join $ execParser $ info (helper <*> mode) mempty
