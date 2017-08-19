{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Foldable
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Lazy as HM.Lazy
import qualified Data.Text as T
import Data.List.Split (chunksOf)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField

import Control.Concurrent.Async
import Pipes
import Pipes.Concurrent as PC
import CAR.Types
import CAR.Utils

import System.Environment

main :: IO ()
main = do
    [path] <- getArgs
    toPostgres ci path

ci :: ConnectInfo
ci = defaultConnectInfo { connectHost = "ben-server.local"
                        , connectUser = "ben"
                        , connectPassword = "mudpie"
                        , connectDatabase = "wikipedia"
                        }

createSchema :: [Query]
createSchema =
    [ [sql| CREATE TABLE IF NOT EXISTS fragments
               ( id serial PRIMARY KEY
               , title text NOT NULL
               , parent integer REFERENCES fragments (id)
               ) |]
    , [sql| CREATE TABLE IF NOT EXISTS paragraphs
               ( id serial PRIMARY KEY
               , paragraph_id text NOT NULL
               , fragment integer REFERENCES fragments (id)
               , content text
               , content_index tsvector
               ) |]
    , [sql| CREATE TABLE IF NOT EXISTS links
               ( src_fragment integer NOT NULL REFERENCES fragments (id)
               , dest_fragment integer NOT NULL REFERENCES fragments (id)
               , paragraph integer NOT NULL REFERENCES paragraphs (id)
               , anchor text
               ) |]
    , [sql| CREATE VIEW links_view AS
            SELECT src.title AS src_title,
                   dest.title AS dest_title,

                   anchor
            FROM links, fragments AS src, fragments AS dest
            WHERE links.src_fragment = src.id
              AND links.dest_fragment = dest.id
            |]
    , [sql| CREATE INDEX ON fragments (title)  |]
    , [sql| CREATE INDEX ON paragraphs (paragraph_id)  |]
    , [sql| CREATE INDEX ON paragraphs (content_index)  |]
    ]

newtype FragmentId = FragmentId Int
                deriving (Eq, Ord, Hashable, Enum, ToField, FromField)
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

toPostgres :: ConnectInfo -> FilePath -> IO ()
toPostgres connInfo pagesFile = do
    conn <- connect connInfo
    mapM_ (execute_ conn) createSchema

    fragments <- pagesToFragments <$> readCborList pagesFile
    let lookupFragmentId :: SectionPath -> Maybe FragmentId
        lookupFragmentId path =
            fmap (\(fragId,_) -> fragId) $ HM.lookup path fragments

    putStrLn "exporting fragments..."
    void $ executeMany conn
        [sql| INSERT INTO fragments ( id, title, parent )
              VALUES (?,?,?) |]
        [ (fragId, title, parentId)
        | (path, (fragId, title)) <- HM.toList fragments
        , let parentId = lookupFragmentId =<< sectionPathParent path
        ]

    putStrLn "exporting paragraphs..."
    pages <- readCborList pagesFile
    forM_ pages $ \page ->
       void $ executeMany conn
           [sql| INSERT INTO paragraphs ( paragraph_id, fragment, content, content_index)
                 SELECT x.column1, x.column2, x.column3, to_tsvector(x.column4)
                 FROM (VALUES (?,?,?,?)) AS x |]
           [ (paraId para, fragId, text, text)
           | (path, _, skel) <- pageSections page
           , Para para <- skel
           , let text = paraToText para
                 fragId = lookupFragmentId path
           ]

    putStrLn "exporting links..."
    (sq, rq, seal) <- PC.spawn' PC.unbounded
    writer <- async $ runEffect $ for (PC.fromInput rq) $ \pagesChunk ->
        let rows :: [(FragmentId, FragmentId, T.Text, ParagraphId)]
            rows =
                [ (srcId, destId, linkAnchor, paraId para)
                | page                <- pagesChunk
                , (srcPath, _, skels) <- pageSections page
                , Just srcId          <- pure $ lookupFragmentId srcPath
                , Para para           <- skels
                , Link{..}            <- paraLinks para
                , let destPath         = SectionPath linkTargetId [] -- TODO section
                , Just destId         <- pure $ lookupFragmentId destPath
                ]
        in void $ liftIO $ executeMany conn
                [sql| INSERT INTO links (src_fragment, dest_fragment, paragraph, anchor)
                      SELECT column1 AS src_fragment, column2 AS dest_fragment, paragraphs.id, column3 AS anchor
                      FROM (VALUES (?,?,?,?)) AS x, paragraphs
                      WHERE paragraphs.paragraph_id = x.column4 |]
                rows
    link writer
    pages <- readCborList pagesFile
    mapM_ (atomically . PC.send sq) (chunksOf 10000 pages)
    atomically seal
    wait writer
    return ()
