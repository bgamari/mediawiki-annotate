{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Concurrent
import Control.Exception
import Control.DeepSeq
import Control.Monad
import Data.Hashable
import Data.Monoid
import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Lazy as HM.Lazy
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Char8 as BS
import Data.List.Split (chunksOf)
import System.FilePath

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField

import Options.Applicative

import NLP.WordNet as WordNet

main :: IO ()
main = do
    let opts = (,) <$> option str (short 'c' <> long "connect" <> help "PostgreSQL connection string")
                   <*> argument str (help "WordNet dictionary directory")
    (connStr, path) <- execParser $ info (helper <*> opts) mempty
    let openConn = connectPostgreSQL (BS.pack connStr)
    toPostgres openConn path

createTables :: [Query]
createTables =
    [ [sql| CREATE UNLOGGED TABLE IF NOT EXISTS synsets
               ( id integer PRIMARY KEY
               , dict_offset integer NOT NULL
               , pos char NOT NULL
               , words array(text) NOT NULL
               )
      |]

    , [sql| CREATE UNLOGGED TABLE IF NOT EXISTS synset_mentions
               ( synset_id integer REFERENCES synsets (id)
               , paragraph_id text REFERENCES paragraphs (paragraph_id)
               )
      |]
    ]

toPostgres :: IO Connection -> FilePath -> IO ()
toPostgres openConn dictPath = do
    conn <- openConn
    mapM_ (execute_ conn) createTables

    mapM_ (exportSynsets . (dictPath </>)) [ "data.verb", "data.noun", "data.adv", "data.adj" ]
  where
    exportSynsets dbFile = do
        conn <- openConn
        synsets <- WordNet.iterSynsets dbFile
        void $ executeMany
            conn
            [sql| INSERT INTO synsets (dict_offset, pos, words)
                  SELECT x.column1, x.column2, x.column3
                  FROM (VALUES (?,?,?)) AS x |]
            (map synsetToRow synsets)

    synsetToRow :: WordNet.Synset -> (Int, String, PGArray T.Text)
    synsetToRow Synset{ssOffset=Offset off, ..} =
        (off, [toPosChar ssPos], PGArray $ map (TE.decodeUtf8 . ssWord) ssWords)

    toPosChar Verb = 'v'
    toPosChar Noun = 'n'
    toPosChar Adj  = 'j'
    toPosChar Adv  = 'r'
