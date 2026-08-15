{-# LANGUAGE OverloadedStrings #-}

-- | Schema for persisting srtree e-graphs.
--
-- Layout (shared by the SQLite and PostgreSQL backends):
--   * @meta@      - scalar settings (@next_id@, @track_dbs@, cost-function tag)
--   * @enode@     - content-addressable e-nodes (@key@ = canonical serialization)
--   * @enode_child@ - ENAry multiset children (@child_eid@, @cnt@)
--   * @eclass@    - e-class id -> canonical representative + height
--   * @eclass_node@ - canonical e-node -> e-class membership
--   * @parent@    - reverse edges: child e-class -> (parent e-class, parent e-node)
--   * @fit@       - per-class risk metrics (fitness, dl, size, theta)
--
-- 'schemaSQL' is the SQLite DDL; 'Algorithm.EqSat.Storage.Postgres' carries
-- the equivalent PostgreSQL DDL (identity keys, deferred FK checks,
-- @DOUBLE PRECISION@). Dataset-specific fit tables are a later phase.
module Algorithm.EqSat.Storage.Schema
  ( schemaSQL
  , createSchema
  ) where

import Data.Text (Text)

import Algorithm.EqSat.Storage.Backend (SqlBackend(..))

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
  , "CREATE TABLE IF NOT EXISTS parent ("
    <> " child_eid INTEGER NOT NULL REFERENCES eclass(eid) ON DELETE CASCADE,"
    <> " parent_eid INTEGER NOT NULL,"
    <> " parent_enode_key TEXT NOT NULL REFERENCES enode(key) ON DELETE CASCADE,"
    <> " PRIMARY KEY (child_eid, parent_eid, parent_enode_key))"
  , "CREATE TABLE IF NOT EXISTS fit ("
    <> " eid INTEGER PRIMARY KEY REFERENCES eclass(eid) ON DELETE CASCADE,"
    <> " fitness REAL,"
    <> " dl REAL,"
    <> " theta TEXT,"
    <> " size INTEGER NOT NULL DEFAULT 0)"
  , "CREATE INDEX IF NOT EXISTS idx_fit_fitness ON fit(fitness)"
  , "CREATE INDEX IF NOT EXISTS idx_fit_size ON fit(size)"
  , "CREATE INDEX IF NOT EXISTS idx_fit_dl ON fit(dl)"
  , "CREATE TABLE IF NOT EXISTS cstore_page ("
    <> " key TEXT PRIMARY KEY,"
    <> " blob TEXT NOT NULL)"
  ]

-- | Create (or ensure) the schema on the given backend.
createSchema :: SqlBackend db => db -> IO ()
createSchema = createSchemaDb
