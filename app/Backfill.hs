{-# LANGUAGE OverloadedStrings #-}

-- | Backfill the @enode_parent@ reverse index for pre-existing databases.
--
-- The @enode_parent@ table is populated during import and eqsat write-through,
-- but databases created before this feature was added will have an empty table.
-- This command scans all @(eid, enode_key)@ pairs, parses each key to extract
-- children, and inserts the reverse-index rows.
module Backfill
  ( BackfillOpts
  , backfillParser
  , runBackfill
  ) where

import Control.Monad (forM_, foldM)
import qualified Data.IntMap as IntMap
import qualified Data.Text as T
import Options.Applicative
import Database.SQLite3 (Database, open, close)

import Algorithm.EqSat.Storage.Backend (SqlBackend, SqlValue(..), execDb, queryDb, runDb, sqlToInt, sqlToText)
import Algorithm.EqSat.Storage.SQLite ()  -- SqlBackend Database instance
import Algorithm.EqSat.Storage.Types (parseEnodeKey)
import Algorithm.EqSat.Egraph (ENode(..))

data BackfillOpts = BackfillOpts
  { backfillDb :: String
  } deriving (Show)

backfillParser :: Parser BackfillOpts
backfillParser = BackfillOpts
  <$> strOption (long "db" <> metavar "FILE" <> help "SQLite database file")

runBackfill :: BackfillOpts -> IO ()
runBackfill (BackfillOpts dbFile) = do
  putStrLn $ "Backfilling enode_parent for " ++ dbFile ++ "..."
  db <- open (T.pack dbFile)
  -- Ensure the table exists
  execDb db
    "CREATE TABLE IF NOT EXISTS enode_parent (\
    \ child_eid INTEGER NOT NULL,\
    \ enode_key TEXT NOT NULL,\
    \ parent_eid INTEGER NOT NULL,\
    \ PRIMARY KEY (child_eid, enode_key))"
  -- Count existing rows
  existing <- countRows db "enode_parent"
  putStrLn $ "  Existing enode_parent rows: " ++ show existing
  -- Scan all (eid, enode_key) pairs
  rows <- queryDb db "SELECT eid, enode_key FROM eclass_node" []
  let pairs = [ (sqlToInt eid, sqlToText key) | [eid, key] <- rows ]
  putStrLn $ "  Total eclass_node pairs: " ++ show (length pairs)
  -- For each pair, parse the key to extract children and insert parent rows
  execDb db "BEGIN"
  count <- foldM (\acc (eid, key) -> do
    case parseEnodeKey (T.unpack key) of
      Nothing -> pure acc
      Just en -> do
        let children = allChildren en
        forM_ children $ \childEid ->
          runDb db "INSERT OR IGNORE INTO enode_parent (child_eid, enode_key, parent_eid) VALUES (?, ?, ?)"
            [ SqlInteger (fromIntegral childEid)
            , SqlText key
            , SqlInteger (fromIntegral eid) ]
        pure (acc + length children)) 0 pairs
  execDb db "COMMIT"
  putStrLn $ "  Inserted " ++ show count ++ " enode_parent rows"
  -- Count final rows
  final <- countRows db "enode_parent"
  putStrLn $ "  Final enode_parent rows: " ++ show final
  close db
  putStrLn "Done."

countRows :: SqlBackend db => db -> String -> IO Int
countRows db table = do
  rows <- queryDb db ("SELECT COUNT(*) FROM " <> T.pack table) []
  pure $ case rows of
    [[n]] -> sqlToInt n
    _     -> 0

allChildren :: ENode -> [Int]
allChildren (EUni _ c)   = [c]
allChildren (EBin _ l r) = [l, r]
allChildren (ENAry _ m)  = map fst $ IntMap.toList m
allChildren _            = []
