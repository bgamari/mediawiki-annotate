{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

main :: IO ()
main = do
    toPostgres ci "pages.cbor"

ci :: ConnectInfo
ci = defaultConnectInfo { connectHost = "localhost"
                        , connectUser = "ben"
                        , connectPassword = "mudpie"
                        , connectDatabase = "wikipedia"
                        }

createSchema :: [Query]
createSchema =
    [ [sql| CREATE TABLE IF NOT EXISTS fragments
               ( fragment_id serial
               , title text NOT NULL
               , parent integer
               ) |]
    , [sql| CREATE TABLE IF NOT EXISTS links
               ( src_fragment_id integer NOT NULL
               , dest_fragment_id integer NOT NULL
               , anchor text
               ) |]
    ]

newtype FragmentId = FragmentId Int
                deriving (Eq, Ord, Hashable, Enum, ToField, FromField)
deriving instance ToField PageName

pagesToFragments :: [Page] -> HM.Lazy.HashMap SectionPath (FragmentId, T.Text, Maybe FragmentId)
pagesToFragments pages =
    let fragments =
          HM.Lazy.fromList [ (path, (pageIdx, title, parentId))
                            | page@Page{..} <- pages
                            , (title, path, parentId) <-
                                  (getPageName pageName, SectionPath pageId [], Nothing)
                                  : [ (getSectionHeading $ last heading, path, Just parentId)
                                    | (path, heading, _) <- pageSections page
                                    , let SectionPath _ hds = path
                                          parent = SectionPath pageId (init hds)
                                    , Just (parentId,_,_) <- pure $ HM.lookup parent fragments
                                    ]
                            | pageIdx <- [FragmentId 0..]
                            ]
    in fragments

toPostgres :: ConnectInfo -> FilePath -> IO ()
toPostgres connInfo pagesFile = do
    conn <- connect connInfo
    mapM_ (execute_ conn) createSchema

    fragments <- pagesToFragments <$> readCborList pagesFile

    void $ executeMany conn
        [sql| INSERT INTO fragments ( fragment_id, title, parent )
              VALUES (?,?,?) |]
        (HM.elems fragments)

    let lookupFragmentId :: SectionPath -> Maybe FragmentId
        lookupFragmentId path =
            fmap (\(fragId,_,_) -> fragId) $ HM.lookup path fragments
    (sq, rq, seal) <- PC.spawn' PC.unbounded
    writer <- async $ runEffect $ for (PC.fromInput rq) $ \pagesChunk ->
        let rows :: [(FragmentId, FragmentId, T.Text)]
            rows =
                [ (srcId, destId, linkAnchor)
                | page                <- pagesChunk
                , (srcPath, _, skels) <- pageSections page
                , Just srcId          <- pure $ lookupFragmentId srcPath
                , skel                <- skels
                , Link{..}            <- pageSkeletonLinks skel
                , let destPath         = (SectionPath linkTargetId []) -- TODO section
                , Just destId         <- pure $ lookupFragmentId destPath
                ]
        in void $ liftIO $ executeMany conn
                [sql| INSERT INTO links (src_fragment_id, dest_fragment_id, anchor)
                      VALUES (?,?,?) |]
                rows
    link writer
    pages <- readCborList pagesFile
    mapM_ (atomically . PC.send sq) (chunksOf 10000 pages)
    atomically seal
    wait writer
    return ()
