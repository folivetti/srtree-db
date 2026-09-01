{-# LANGUAGE OverloadedStrings #-}

-- | Schema for persisting srtree e-graphs.
--
-- Two logical sections, potentially in separate DB files:
--   * E-graph section (dataset-agnostic, read-only during fitting):
--     @meta@, @enode@, @enode_child@, @eclass@, @eclass_node@,
--     @cstore_page@, @frontier@
--   * Dataset-fit section (per-dataset, write-heavy during fitting):
--     @dataset@, @dataset_fit@, @expression_index@
--
-- 'egraphSchemaSQL' is the DDL for the egraph DB.
-- 'fitSchemaSQL' is the DDL for a per-dataset fit DB (no FK to eclass).
module Algorithm.EqSat.Storage.Schema
  ( egraphSchemaSQL
  , fitSchemaSQL
  , createSchema
  , createSchemaFit
  ) where

import Data.Text (Text)

import Algorithm.EqSat.Storage.Backend (SqlBackend(..))

-- | DDL for the egraph database.
-- Full schema including dataset tables for backward compatibility with importEqs.
egraphSchemaSQL :: [Text]
egraphSchemaSQL =
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
  , "CREATE TABLE IF NOT EXISTS cstore_page ("
    <> " key INTEGER PRIMARY KEY,"
    <> " blob BLOB NOT NULL)"
  , "CREATE TABLE IF NOT EXISTS frontier ("
    <> " eid INTEGER PRIMARY KEY REFERENCES eclass(eid) ON DELETE CASCADE,"
    <> " updated_at TEXT)"
  , "CREATE TABLE IF NOT EXISTS dataset ("
    <> " id INTEGER PRIMARY KEY,"
    <> " name TEXT NOT NULL UNIQUE,"
    <> " created TEXT)"
  , "CREATE TABLE IF NOT EXISTS dataset_fit ("
    <> " dataset_id INTEGER NOT NULL REFERENCES dataset(id) ON DELETE CASCADE,"
    <> " eid INTEGER NOT NULL REFERENCES eclass(eid) ON DELETE CASCADE,"
    <> " fitness REAL,"
    <> " dl REAL,"
    <> " theta TEXT,"
    <> " size INTEGER NOT NULL DEFAULT 0,"
    <> " evaluated INTEGER NOT NULL DEFAULT 0,"
    <> " fitted INTEGER NOT NULL DEFAULT 0,"
    <> " stale INTEGER NOT NULL DEFAULT 0,"
    <> " updated_at TEXT,"
    <> " PRIMARY KEY (dataset_id, eid))"
  , "CREATE TABLE IF NOT EXISTS expression_index ("
    <> " expression_key TEXT PRIMARY KEY,"
    <> " eclass INTEGER NOT NULL REFERENCES eclass(eid) ON DELETE CASCADE,"
    <> " dataset_id INTEGER REFERENCES dataset(id) ON DELETE CASCADE,"
    <> " first_seen TEXT)"
  ]

-- | DDL for a per-dataset fit database.
-- No FK to eclass (e-graph lives in a separate DB).
fitSchemaSQL :: [Text]
fitSchemaSQL =
  [ "CREATE TABLE IF NOT EXISTS dataset ("
    <> " id INTEGER PRIMARY KEY,"
    <> " name TEXT NOT NULL UNIQUE,"
    <> " created TEXT)"
  , "CREATE TABLE IF NOT EXISTS dataset_fit ("
    <> " dataset_id INTEGER NOT NULL REFERENCES dataset(id) ON DELETE CASCADE,"
    <> " eid INTEGER NOT NULL,"
    <> " fitness REAL,"
    <> " dl REAL,"
    <> " theta TEXT,"
    <> " size INTEGER NOT NULL DEFAULT 0,"
    <> " evaluated INTEGER NOT NULL DEFAULT 0,"
    <> " fitted INTEGER NOT NULL DEFAULT 0,"
    <> " stale INTEGER NOT NULL DEFAULT 0,"
    <> " updated_at TEXT,"
    <> " PRIMARY KEY (dataset_id, eid))"
  , "CREATE TABLE IF NOT EXISTS expression_index ("
    <> " expression_key TEXT PRIMARY KEY,"
    <> " eclass INTEGER NOT NULL,"
    <> " dataset_id INTEGER REFERENCES dataset(id) ON DELETE CASCADE,"
    <> " first_seen TEXT)"
  ]

-- | Create (or ensure) the egraph schema on the given backend.
createSchema :: SqlBackend db => db -> IO ()
createSchema = createSchemaDb

-- | Create (or ensure) the fit schema on the given backend.
createSchemaFit :: SqlBackend db => db -> IO ()
createSchemaFit = createSchemaDbFit
