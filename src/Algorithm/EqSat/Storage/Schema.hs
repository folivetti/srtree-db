{-# LANGUAGE OverloadedStrings #-}

-- | SQLite schema for persisting srtree e-graphs.
--
-- Layout:
--   * @meta@      - scalar settings (@next_id@, @track_dbs@, cost-function tag)
--   * @enode@     - content-addressable e-nodes (@key@ = canonical serialization)
--   * @enode_child@ - ENAry multiset children (@child_eid@, @cnt@)
--   * @eclass@    - e-class id -> canonical representative + height
--   * @eclass_node@ - canonical e-node -> e-class membership
--   * @fit@       - per-class risk metrics (fitness, dl, size, theta)
--
-- Per-dataset fit tables and a PostgreSQL backend are added at a later phase.
module Algorithm.EqSat.Storage.Schema
  ( schemaSQL
  , createSchema
  ) where

import Database.SQLite3 (Database, exec)
import Data.Text (Text)

schemaSQL :: [Text]
schemaSQL =
  [ "CREATE TABLE IF NOT EXISTS meta ("
    <> " key TEXT PRIMARY KEY,"
    <> " value TEXT NOT NULL)"
  , "CREATE TABLE IF NOT EXISTS enode ("
    <> " key TEXT PRIMARY KEY,"
    <> " op TEXT NOT NULL,"
    <> " op_detail TEXT,"
    <> " a INTEGER,"
    <> " b INTEGER,"
    <> " x REAL)"
  , "CREATE TABLE IF NOT EXISTS enode_child ("
    <> " enode_key TEXT NOT NULL REFERENCES enode(key) ON DELETE CASCADE,"
    <> " child_eid INTEGER NOT NULL,"
    <> " cnt INTEGER NOT NULL DEFAULT 1,"
    <> " PRIMARY KEY (enode_key, child_eid))"
  , "CREATE TABLE IF NOT EXISTS eclass ("
    <> " eid INTEGER PRIMARY KEY,"
    <> " canonical INTEGER NOT NULL,"
    <> " height INTEGER NOT NULL DEFAULT 0)"
  , "CREATE TABLE IF NOT EXISTS eclass_node ("
    <> " eid INTEGER NOT NULL REFERENCES eclass(eid) ON DELETE CASCADE,"
    <> " enode_key TEXT NOT NULL REFERENCES enode(key) ON DELETE CASCADE,"
    <> " PRIMARY KEY (eid, enode_key))"
  , "CREATE TABLE IF NOT EXISTS fit ("
    <> " eid INTEGER PRIMARY KEY REFERENCES eclass(eid) ON DELETE CASCADE,"
    <> " fitness REAL,"
    <> " dl REAL,"
    <> " theta TEXT,"
    <> " size INTEGER NOT NULL DEFAULT 0)"
  , "CREATE INDEX IF NOT EXISTS idx_fit_fitness ON fit(fitness)"
  , "CREATE INDEX IF NOT EXISTS idx_fit_size ON fit(size)"
  , "CREATE INDEX IF NOT EXISTS idx_fit_dl ON fit(dl)"
  ]

-- | Create (or ensure) the schema on an open database.
createSchema :: Database -> IO ()
createSchema db = mapM_ (exec db) schemaSQL