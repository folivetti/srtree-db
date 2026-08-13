{-# LANGUAGE OverloadedStrings #-}

-- | SQL query API over a stored e-graph's @fit@ / @enode@ tables.
--
-- The queries are intentionally SQL-shaped (this is the slice of the
-- reggression functionality that runs directly in the database) and mirror the
-- in-memory counterparts in 'Algorithm.EqSat.Queries'.
module Algorithm.EqSat.Storage.Query
  ( topN
  , pareto
  , paretoBySize
  , distributionCounts
  , countPattern
  ) where

import Data.Text (Text)
import Database.SQLite3
  ( Database, SQLData(..)
  , withStatement, bind, step, columns )

import Algorithm.EqSat.Egraph (EClassId)

import Algorithm.EqSat.Storage.SQLite as Store
  ( query, sqlToInt, sqlToMaybeDouble )

-- | The @n@ e-classes with the best fitness, descending.
topN :: Database -> Int -> IO [(EClassId, Double)]
topN db n = do
  rows <- Store.query db
    "SELECT eid, fitness FROM fit WHERE fitness IS NOT NULL \
    \ORDER BY fitness DESC LIMIT ?"
    [ SQLInteger (fromIntegral n) ]
  pure [ (Store.sqlToInt eid, f)
       | [eid, f] <- rows
       , Just f   <- [Store.sqlToMaybeDouble f] ]

-- | Non-dominated classes over (max fitness, min dl). Returns the (eid,
-- fitness, dl) triples that are not dominated by any other class.
pareto :: Database -> IO [(EClassId, Double, Double)]
pareto db = do
  rows <- Store.query db
    "SELECT eid, fitness, dl FROM fit \
    \WHERE fitness IS NOT NULL AND dl IS NOT NULL" []
  let pts = [ (Store.sqlToInt eid, f, d)
            | [eid, ff, dd] <- rows
            , Just f <- [Store.sqlToMaybeDouble ff]
            , Just d <- [Store.sqlToMaybeDouble dd] ]
      dominates (f1, d1) (f0, d0) = f1 >= f0 && d1 <= d0 && (f1 > f0 || d1 < d0)
      nonDominated (eid_, f, d) = not (any (\(q0, qf, qd) -> dominates (qf, qd) (f, d)) pts)
  pure [ p | p@(_, f, d) <- pts, nonDominated p ]

-- | Non-dominated classes over (max fitness, min size), matching the
-- in-memory pareto front. Returns the (eid, fitness, size) triples that are
-- not dominated by any other evaluated class.
paretoBySize :: Database -> IO [(EClassId, Double, Int)]
paretoBySize db = do
  rows <- Store.query db
    "SELECT eid, fitness, size FROM fit WHERE fitness IS NOT NULL" []
  let pts = [ (Store.sqlToInt eid, f, s)
            | [eid, ff, ss] <- rows
            , Just f <- [Store.sqlToMaybeDouble ff]
            , let s = Store.sqlToInt ss ]
      dominates (f1, s1) (f0, s0) = f1 >= f0 && s1 <= s0 && (f1 > f0 || s1 < s0)
      nonDominated (eid_, f, s) = not (any (\(q0, qf, qs) -> dominates (qf, qs) (f, s)) pts)
  pure [ p | p@(_, f, s) <- pts, nonDominated p ]

-- | Number of evaluated e-classes per model size (up to @maxSize@, inclusive).
distributionCounts :: Database -> Int -> IO [(Int, Int)]
distributionCounts db maxSize = do
  rows <- Store.query db
    "SELECT size, COUNT(*) FROM fit \
    \WHERE fitness IS NOT NULL AND size <= ? \
    \GROUP BY size ORDER BY size"
    [ SQLInteger (fromIntegral maxSize) ]
  pure [ (Store.sqlToInt s, Store.sqlToInt c) | [s, c] <- rows ]

-- | Number of distinct e-classes containing at least one e-node whose
-- specific operator matches (e.g. \"EAdd\", \"EMul\", \"Add\", \"LogAbs\").
countPattern :: Database -> Text -> IO Int
countPattern db op = do
  rows <- Store.query db
    "SELECT COUNT(DISTINCT eclass_node.eid) \
    \FROM eclass_node JOIN enode ON enode.key = eclass_node.enode_key \
    \WHERE enode.op_detail = ?"
    [ SQLText op ]
  case rows of
    [[SQLInteger n]] -> pure (fromIntegral n)
    _                -> pure 0