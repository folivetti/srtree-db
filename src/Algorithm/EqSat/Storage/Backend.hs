{-# LANGUAGE OverloadedStrings #-}

-- | Driver-neutral SQL backend for srtree e-graph persistence.
--
-- The storage layer ('Algorithm.EqSat.Storage.SQLite' and
-- 'Algorithm.EqSat.Storage.Postgres') is written against this tiny interface
-- instead of a concrete driver, so the same serialization, import and query
-- code runs on SQLite and PostgreSQL. Drivers only provide:
--
--   * 'execDb'  - statements without parameters (DDL, BEGIN/COMMIT, DELETE)
--   * 'runDb'   - parameterized statements that return no rows (INSERT/UPDATE)
--   * 'queryDb' - parameterized statements returning rows (SELECT)
--   * 'createSchemaDb' - create the driver-specific schema
--
-- Parameters use the positional @?@ placeholder in the shared SQL; the
-- PostgreSQL driver rewrites them to @$n@. NULL is expressed as the literal
-- @NULL@ in the shared SQL (never as a parameter), so drivers do not need a
-- NULL parameter representation.
module Algorithm.EqSat.Storage.Backend
  ( SqlValue(..)
  , SqlBackend(..)
  , sqlToInt
  , sqlToMaybeDouble
  , sqlToText
  , sqlToBlob
  ) where

import Data.Int (Int64)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import Algorithm.EqSat.Egraph (EClassId)

-- | A driver-neutral value bound to a @?@ parameter or returned by a query.
data SqlValue = SqlInteger Int64
              | SqlFloat   Double
              | SqlText    Text
              | SqlBlob    BS.ByteString
              | SqlNull
  deriving (Eq, Show)

-- | The minimal SQL surface used by the storage layer.
class SqlBackend db where
  -- | Execute a statement without parameters.
  execDb :: db -> Text -> IO ()
  -- | Execute a parameterized statement that returns no rows.
  runDb :: db -> Text -> [SqlValue] -> IO ()
  -- | Execute an idempotent insert, ignoring any row that would violate a
  -- primary/unique constraint (re-seeing a node already present). The @Text@
  -- argument is the @table (cols) VALUES (?,...)@ tail *without* the leading
  -- @INSERT INTO@ and without a conflict clause; each driver supplies its own
  -- native prefix/suffix (SQLite @INSERT OR IGNORE INTO@, Postgres @ON
  -- CONFLICT DO NOTHING@).
  insertIgnore :: db -> Text -> [SqlValue] -> IO ()
  -- | Run a parameterized query and return the raw result grid.
  queryDb :: db -> Text -> [SqlValue] -> IO [[SqlValue]]
  -- | Fold over query results one row at a time without materializing the
  -- full result list. This is O(1) memory in the row accumulator (unlike
  -- 'queryDb' which builds a spine-strict list of all rows).
  foldQueryDb :: db -> Text -> [SqlValue] -> a -> (a -> [SqlValue] -> IO a) -> IO a
  -- | Stream (bounded) the distinct e-class ids whose e-class contains a node
  -- with the given @op_detail@, for the streaming matcher, skipping any ids in
  -- @exclude@ (the already-attempted seen-set, so the per-rule budget advances
  -- to new roots across scheduler cycles). Drivers that expose a cursor
  -- ('Database.SQLite3') implement this O(1)-memory; others fall back to a grid
  -- 'queryDb' (unbounded, documented).
  streamByOp :: db -> Text -> Int -> [EClassId] -> IO [EClassId]
  -- | Stream the @key, blob@ rows of a key-value page table (e.g.
  -- @cstore_page@) to a callback, one at a time, so a full pass over every page
  -- (e.g. 'pushFit') stays O(1) memory. Drivers with a cursor ('Database.SQLite3')
  -- implement this; others fall back to a grid 'queryDb' (unbounded, documented).
  -- The blob is delivered hex-decoded (raw) to the callback.
  streamPages :: db -> Text -> (Int64 -> BS.ByteString -> IO ()) -> IO ()
  -- | Create the schema (tables, indexes) for this driver.
  createSchemaDb :: db -> IO ()

sqlToInt :: SqlValue -> Int
sqlToInt (SqlInteger n) = fromIntegral n
sqlToInt (SqlText t)    = fromMaybe 0 (listToMaybe [ i | (i, "") <- reads (T.unpack t) ])
sqlToInt _              = 0

sqlToMaybeDouble :: SqlValue -> Maybe Double
sqlToMaybeDouble SqlNull        = Nothing
sqlToMaybeDouble (SqlFloat d)   = Just d
sqlToMaybeDouble (SqlInteger n) = Just (fromIntegral n)
sqlToMaybeDouble (SqlText t)    = case reads (T.unpack t) of
                                    [(d, "")] -> Just d
                                    _         -> Nothing
sqlToMaybeDouble _ = Nothing

sqlToText :: SqlValue -> Text
sqlToText (SqlText t)    = t
sqlToText (SqlInteger n) = T.pack (show n)
sqlToText (SqlFloat d)   = T.pack (show d)
sqlToText (SqlBlob bs)   = TE.decodeUtf8 bs
sqlToText SqlNull        = ""

sqlToBlob :: SqlValue -> BS.ByteString
sqlToBlob (SqlBlob bs) = bs
sqlToBlob (SqlText t)  = TE.encodeUtf8 t
sqlToBlob _            = BS.empty
