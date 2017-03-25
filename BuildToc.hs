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
    $  command "pages" (info indexPages fullDesc)
  where
    indexPages =
        void . Toc.buildIndex pageId <$> argument str (help "pages file" <> metavar "FILE")

main :: IO ()
main = join $ execParser $ info (helper <*> mode) mempty
