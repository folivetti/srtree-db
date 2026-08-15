{-# LANGUAGE OverloadedStrings #-}

-- | SQL query API over a stored e-graph's @fit@ / @enode@ tables.
--
-- The queries are intentionally SQL-shaped (this is the slice of the
-- reggression functionality that runs directly in the database) and mirror the
-- in-memory counterparts in 'Algorithm.EqSat.Queries'. They are written
-- against 'Algorithm.EqSat.Storage.Backend', so the same SQL drives the
-- SQLite and PostgreSQL backends.
module Algorithm.EqSat.Storage.Query
  ( topN
  , pareto
  , paretoBySize
  , distributionCounts
  , countPattern
  ) where

import Data.Text (Text)

import Algorithm.EqSat.Egraph (EClassId)

import Algorithm.EqSat.Storage.Backend
  ( SqlBackend, SqlValue(..), queryDb, sqlToInt, sqlToMaybeDouble )

-- | The @n@ e-classes with the best fitness, descending.
topN :: SqlBackend db => db -> Int -> IO [(EClassId, Double)]
topN db n = do
  rows <- queryDb db
    "SELECT eid, fitness FROM fit WHERE fitness IS NOT NULL \
    \ORDER BY fitness DESC LIMIT ?"
    [ SqlInteger (fromIntegral n) ]
  pure [ (sqlToInt eid, f)
       | [eid, f] <- rows
       , Just f   <- [sqlToMaybeDouble f] ]

-- | Non-dominated classes over (max fitness, min dl). Returns the (eid,
-- fitness, dl) triples that are not dominated by any other class.
pareto :: SqlBackend db => db -> IO [(EClassId, Double, Double)]
pareto db = do
  rows <- queryDb db
    "SELECT eid, fitness, dl FROM fit \
    \WHERE fitness IS NOT NULL AND dl IS NOT NULL" []
  let pts = [ (sqlToInt eid, f, d)
            | [eid, ff, dd] <- rows
            , Just f <- [sqlToMaybeDouble ff]
            , Just d <- [sqlToMaybeDouble dd] ]
      dominates (f1, d1) (f0, d0) = f1 >= f0 && d1 <= d0 && (f1 > f0 || d1 < d0)
      nonDominated (eid_, f, d) = not (any (\(q0, qf, qd) -> dominates (qf, qd) (f, d)) pts)
  pure [ p | p@(_, f, d) <- pts, nonDominated p ]

-- | Non-dominated classes over (max fitness, min size), matching the
-- in-memory pareto front. Returns the (eid, fitness, size) triples that are
-- not dominated by any other evaluated class.
paretoBySize :: SqlBackend db => db -> IO [(EClassId, Double, Int)]
paretoBySize db = do
  rows <- queryDb db
    "SELECT eid, fitness, size FROM fit WHERE fitness IS NOT NULL" []
  let pts = [ (sqlToInt eid, f, s)
            | [eid, ff, ss] <- rows
            , Just f <- [sqlToMaybeDouble ff]
            , let s = sqlToInt ss ]
      dominates (f1, s1) (f0, s0) = f1 >= f0 && s1 <= s0 && (f1 > f0 || s1 < s0)
      nonDominated (eid_, f, s) = not (any (\(q0, qf, qs) -> dominates (qf, qs) (f, s)) pts)
  pure [ p | p@(_, f, s) <- pts, nonDominated p ]

-- | Number of evaluated e-classes per model size (up to @maxSize@, inclusive).
distributionCounts :: SqlBackend db => db -> Int -> IO [(Int, Int)]
distributionCounts db maxSize = do
  rows <- queryDb db
    "SELECT size, COUNT(*) FROM fit \
    \WHERE fitness IS NOT NULL AND size <= ? \
    \GROUP BY size ORDER BY size"
    [ SqlInteger (fromIntegral maxSize) ]
  pure [ (sqlToInt s, sqlToInt c) | [s, c] <- rows ]

-- | Number of distinct e-classes containing at least one e-node whose
-- specific operator matches (e.g. \"EAdd\", \"EMul\", \"Add\", \"LogAbs\").
countPattern :: SqlBackend db => db -> Text -> IO Int
countPattern db op = do
  rows <- queryDb db
    "SELECT COUNT(DISTINCT eclass_node.eid) \
    \FROM eclass_node JOIN enode ON enode.key = eclass_node.enode_key \
    \WHERE enode.op_detail = ?"
    [ SqlText op ]
  pure $ case rows of
    row : _ | [n] <- row -> sqlToInt n
    _                    -> 0