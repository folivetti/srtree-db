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
  ) where

import Data.Int (Int64)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T

-- | A driver-neutral value bound to a @?@ parameter or returned by a query.
data SqlValue = SqlInteger Int64
              | SqlFloat   Double
              | SqlText    Text
              | SqlNull
  deriving (Eq, Show)

-- | The minimal SQL surface used by the storage layer.
class SqlBackend db where
  -- | Execute a statement without parameters.
  execDb :: db -> Text -> IO ()
  -- | Execute a parameterized statement that returns no rows.
  runDb :: db -> Text -> [SqlValue] -> IO ()
  -- | Run a parameterized query and return the raw result grid.
  queryDb :: db -> Text -> [SqlValue] -> IO [[SqlValue]]
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
sqlToText SqlNull        = ""
