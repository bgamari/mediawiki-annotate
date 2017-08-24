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
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Char8 as BS
import Data.List.Split (chunksOf)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField
import Options.Applicative

import Control.Concurrent.Async
import Pipes
import Pipes.Concurrent as PC
import SimplIR.Utils.Compact
import CAR.Types
import CAR.Utils

main :: IO ()
main = do
    let opts = (,) <$> option str (short 'c' <> long "connect" <> help "PostgreSQL connection string")
                   <*> argument str (help "Articles file")
    (connStr, path) <- execParser $ info (helper <*> opts) mempty
    let openConn = connectPostgreSQL (BS.pack connStr)
    toPostgres openConn path


createTables :: [Query]
createTables =
    [ [sql| CREATE UNLOGGED TABLE IF NOT EXISTS fragments
               ( id serial PRIMARY KEY
               , title text NOT NULL
               , parent integer -- REFERENCES fragments (id)
               )
      |]
    , [sql| CREATE UNLOGGED TABLE IF NOT EXISTS paragraphs
               ( id serial PRIMARY KEY
               , paragraph_id text NOT NULL
               , fragment integer REFERENCES fragments (id)
               , content text
               )
      |]
    , [sql| CREATE UNLOGGED TABLE IF NOT EXISTS links
               ( src_fragment integer NOT NULL REFERENCES fragments (id)
               , dest_fragment integer NOT NULL REFERENCES fragments (id)
               , paragraph integer NOT NULL REFERENCES paragraphs (id)
               , anchor text
               )
      |]
    ]

createViews :: [Query]
createViews =
    [ [sql| CREATE MATERIALIZED VIEW raw_fragment_parents(fragment_id, parent, depth) AS
            WITH RECURSIVE frags(fragment_id, parent, depth) AS (
                SELECT fragments.id AS fragment_id,
                      fragments.parent AS parent,
                      0 AS depth
                FROM fragments
                WHERE fragments.parent IS NOT NULL
              UNION ALL
                SELECT frags.fragment_id, parent.parent, frags.depth+1
                FROM frags, fragments AS parent
                WHERE frags.parent = parent.id
                  AND parent.parent IS NOT NULL
            )
            SELECT * FROM frags
      |]
    , [sql| CREATE VIEW fragment_parents(fragment_id, title, parent_ids, parent_titles) AS
                SELECT fragment_id,  fragments.title, array_agg(parent.id) AS parent_ids, array_agg(parent.title) AS parent_titles
                FROM raw_fragment_parents
                INNER JOIN fragments ON fragments.id = raw_fragment_parents.fragment_id
                INNER JOIN fragments AS parent ON parent.id = raw_fragment_parents.parent
                GROUP BY raw_fragment_parents.fragment_id, fragments.title
      |]
    , [sql| CREATE VIEW links_view AS
            SELECT src.title AS src_title,
                   dest.title AS dest_title,
                   paragraphs.content AS paragraph,
                   anchor,
                   parent.parent_titles
            FROM links,
                 fragments AS src,
                 fragments AS dest,
                 fragment_parents AS parent,
                 paragraphs
            WHERE links.src_fragment = src.id
              AND links.dest_fragment = dest.id
              AND paragraphs.id = links.paragraph
              AND parent.fragment_id = src.id
      |]
    , [sql| CREATE VIEW paragraphs_view AS
            SELECT paragraph_id,
                   content,
                   parent_titles
            FROM paragraphs, fragment_parents
            WHERE fragment_parents.fragment_id = paragraphs.fragment
      |]
    ]

finishSchema :: [Query]
finishSchema =
    [ [sql| ALTER TABLE fragments ADD FOREIGN KEY (parent) REFERENCES fragments(id) |]
    , [sql| ALTER TABLE fragments SET LOGGED |]
    , [sql| ALTER TABLE paragraphs SET LOGGED |]
    , [sql| ALTER TABLE links SET LOGGED |]
    , [sql| CREATE INDEX ON fragments (title)  |]
    , [sql| CREATE INDEX ON paragraphs (paragraph_id)  |]
    , [sql| CREATE INDEX ON paragraphs USING GIN (to_tsvector('english', content)) |]
    , [sql| ANALYZE |]
    ]

newtype FragmentId = FragmentId Int
                deriving (Eq, Ord, Hashable, Enum, ToField, FromField, NFData)
deriving instance ToField PageName
instance ToField ParagraphId where
    toField = toField . unpackParagraphId

pagesToFragments :: [Page] -> HM.Lazy.HashMap SectionPath (FragmentId, T.Text)
pagesToFragments pages =
    HM.Lazy.fromList [ (path, (pageIdx, title))
                     | page@Page{..} <- pages
                     , (title, path) <-
                           (getPageName pageName, SectionPath pageId [])
                           : [ (getSectionHeading $ last heading, path)
                             | (path, heading, _) <- pageSections page
                             ]
                     | pageIdx <- [FragmentId 0..]
                     ]

sectionPathParent :: SectionPath -> Maybe SectionPath
sectionPathParent (SectionPath _      [])  = Nothing
sectionPathParent (SectionPath pageId hds) = Just $ SectionPath pageId (init hds)

insertChunks :: ToRow a => [Connection] -> Query -> [[a]] -> IO ()
insertChunks conns query rowChunks = do
    (sq, rq, seal) <- PC.spawn' $ PC.bounded 200
    inserters <- mapM (startInserter rq) conns
    mapM_ link inserters
    mapM_ (atomically . PC.send sq) rowChunks
    atomically seal
    mapM_ wait inserters
  where
    startInserter rq conn = async $ withTransaction conn $ do
        tid <- myThreadId
        runEffect $ for (PC.fromInput rq) $ \chunk -> do
            liftIO $ print (tid, length chunk)
            void $ liftIO $ executeMany conn query chunk

toPostgres :: IO Connection -> FilePath -> IO ()
toPostgres openConn pagesFile = do
    conn <- openConn
    conns <- replicateM 32 openConn
    mapM_ (execute_ conn) createTables

    putStrLn "building fragment index..."
    !fragments <- pagesToFragments <$> readCborList pagesFile
    let fragmentIdxMap :: HM.HashMap SectionPath FragmentId
        !fragmentIdxMap = inCompact $ HM.map (\(fragId,_) -> fragId) fragments
        lookupFragmentId :: SectionPath -> Maybe FragmentId
        lookupFragmentId path = HM.lookup path fragmentIdxMap

    exportFragments conns fragments lookupFragmentId
    exportParagraphs conns lookupFragmentId
    exportLinks conns lookupFragmentId

    mapM_ (execute_ conn) finishSchema
    mapM_ (execute_ conn) createViews
    return ()

  where
    exportFragments conns fragments lookupFragmentId = do
      putStrLn "exporting fragments..."
      insertChunks conns
          [sql| INSERT INTO fragments ( id, title, parent )
                VALUES (?,?,?) |]
          $ chunksOf 10000
          [ (fragId, title, parentId)
          | (path, (fragId, title)) <- HM.toList fragments
          , let parentId = lookupFragmentId =<< sectionPathParent path
          ]

    exportParagraphs conns lookupFragmentId = do
        putStrLn "exporting paragraphs..."
        pages <- readCborList pagesFile
        let pageParaRows :: Page -> [(ParagraphId, Maybe FragmentId, TL.Text)]
            pageParaRows page =
              [ (paraId para, fragId, text)
              | (path, _, skel) <- pageSections page
              , Para para <- skel
              , let text = paraToText para
                    fragId = lookupFragmentId path
              ]
        insertChunks
            conns
            [sql| INSERT INTO paragraphs ( paragraph_id, fragment, content )
                  SELECT x.column1, x.column2, x.column3
                  FROM (VALUES (?,?,?)) AS x |]
            (map (foldMap pageParaRows) $ chunksOf 100 pages)

    exportLinks conns lookupFragmentId = do
        putStrLn "exporting links..."
        pages <- readCborList pagesFile
        let pagesLinkRows :: [Page] -> [(FragmentId, FragmentId, T.Text, ParagraphId)]
            pagesLinkRows pagesChunk =
                [ (srcId, destId, linkAnchor, paraId para)
                | page                <- pagesChunk
                , (srcPath, _, skels) <- pageSections page
                , Just srcId          <- pure $ lookupFragmentId srcPath
                , Para para           <- skels
                , Link{..}            <- paraLinks para
                , let destPath         = SectionPath linkTargetId [] -- TODO section
                , Just destId         <- pure $ lookupFragmentId destPath
                ]
        insertChunks
            conns
            [sql| INSERT INTO links (src_fragment, dest_fragment, paragraph, anchor)
                  SELECT column1 AS src_fragment, column2 AS dest_fragment, paragraphs.id, column3 AS anchor
                  FROM (VALUES (?,?,?,?)) AS x, paragraphs
                  WHERE paragraphs.paragraph_id = x.column4 |]
            (map pagesLinkRows $ chunksOf 1000 pages)

