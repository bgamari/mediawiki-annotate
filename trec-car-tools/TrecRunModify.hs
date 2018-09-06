{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Monoid
import Options.Applicative
import Control.Monad
import Control.DeepSeq
import Data.Maybe
import Data.Hashable
import Codec.Serialise as CBOR
import qualified Data.Text as T

import CAR.Types
import CAR.TocFile as Toc
import CAR.ToolVersion
import CAR.RunFile
import CAR.RunFile as RF

import SimplIR.Format.TrecRunFile as TrecRunFile


opts :: Parser (IO ())
opts = subparser
    $  cmd "entity-drop-paragraphs"   entityDropParagraphs'
  where
    cmd name action = command name (info (helper <*> action) fullDesc)

    entityDropParagraphs' =
        entityDropParagraphs
            <$> argument str (help "Input run file" <> metavar "RUNFILE")
            <*> option str (long "output" <> short 'o' <> help "Output run file" <> metavar "RUNFILE")



    entityDropParagraphs inputRunFile outputRunFile = do
        rankings <- readEntityParagraphRun inputRunFile :: IO [RF.PassageEntityRankingEntry]
        let filteredRankings = map dropParagraphFromEntityRankEntry rankings
        writeEntityRun outputRunFile filteredRankings



      where -- docNameToTocKey =
            --  packPageId .  T.unpack
            dropParagraphFromEntityRankEntry :: PassageEntityRankingEntry -> EntityRankingEntry
            dropParagraphFromEntityRankEntry  r =
                let entityName = carEntity r --(carDocument r)
                in r {carDocument = entityName}




main :: IO ()
main = join $ execParser' 1 (helper <*> opts) mempty
