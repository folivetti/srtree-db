{-# LANGUAGE OverloadedStrings #-}

-- | SQL query API over a stored e-graph's @fit@ / @enode@ tables.
--
-- The queries are intentionally SQL-shaped (this is the slice of the
-- reggression functionality that runs directly in the database) and mirror the
-- in-memory counterparts in 'Algorithm.EqSat.Queries'. They are written
-- against 'Algorithm.EqSat.Storage.Backend', so the same SQL drives the
-- SQLite and PostgreSQL backends.
module Algorithm.EqSat.Storage.Query
  ( getOrCreateDataset
  , datasetId
  , writeDatasetFit
  , readDatasetFit
  , topN
  , pareto
  , paretoBySize
  , distributionCounts
  , countPattern
  , expressionEclass
  , testedOnDataset
  , versionsOf
  ) where

import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T

import Algorithm.EqSat.Egraph (EClassId)

import Algorithm.EqSat.Storage.Backend
  ( SqlBackend, SqlValue(..), runDb, queryDb, sqlToInt, sqlToMaybeDouble, sqlToText )
import Algorithm.EqSat.Storage.Schema (createSchema)

-- | Resolve a dataset name to its @dataset@ row id, creating it if needed.
getOrCreateDataset :: SqlBackend db => db -> String -> IO Int
getOrCreateDataset db name = do
  createSchema db
  m <- datasetId db name
  case m of
    Just i  -> pure i
    Nothing -> do
      runDb db "INSERT INTO dataset (name) VALUES (?)" [SqlText (T.pack name)]
      r <- datasetId db name
      pure (maybe 0 id r)

-- | Look up an existing dataset id by name.
datasetId :: SqlBackend db => db -> String -> IO (Maybe Int)
datasetId db name = do
  rows <- queryDb db "SELECT id FROM dataset WHERE name = ?" [SqlText (T.pack name)]
  pure $ case rows of
    ([i] : _) -> Just (sqlToInt i)
    _         -> Nothing

-- | Upsert a per-(dataset, e-class) fit row.
writeDatasetFit
  :: SqlBackend db => db -> Int -> EClassId
  -> Maybe Double -> Maybe Double -> Text -> Int -> IO ()
writeDatasetFit db ds eid fit dl theta sz = do
  let (fitCol, fitVal) = case fit of
        Nothing -> ("NULL", Nothing)
        Just f  -> ("?", Just (SqlFloat f))
      (dlCol, dlVal) = case dl of
        Nothing -> ("NULL", Nothing)
        Just d  -> ("?", Just (SqlFloat d))
  runDb db
    ("INSERT OR REPLACE INTO dataset_fit \
     \(dataset_id, eid, fitness, dl, theta, size, evaluated, fitted) \
     \VALUES (?, ?, " <> fitCol <> ", " <> dlCol <> ", ?, ?, 1, 1)")
    (catMaybes [ Just (SqlInteger (fromIntegral ds))
               , Just (SqlInteger (fromIntegral eid))
               , fitVal
               , dlVal
               , Just (SqlText theta)
               , Just (SqlInteger (fromIntegral sz)) ])

-- | Read per-(dataset, e-class) fit rows.
readDatasetFit
  :: SqlBackend db => db -> Int
  -> IO [(EClassId, (Maybe Double, Maybe Double, Int, Text))]
readDatasetFit db ds = do
  createSchema db
  rows <- queryDb db
    "SELECT eid, fitness, dl, size, theta FROM dataset_fit WHERE dataset_id = ?"
    [SqlInteger (fromIntegral ds)]
  pure [ (sqlToInt eid, (sqlToMaybeDouble f, sqlToMaybeDouble d, sqlToInt sz, sqlToText th))
       | [eid, f, d, sz, th] <- rows ]

-- | The @n@ e-classes with the best fitness (on the dataset), descending.
topN :: SqlBackend db => db -> Int -> Int -> IO [(EClassId, Double)]
topN db ds n = do
  rows <- queryDb db
    "SELECT eid, fitness FROM dataset_fit \
    \WHERE dataset_id = ? AND fitness IS NOT NULL \
    \ORDER BY fitness DESC LIMIT ?"
    [ SqlInteger (fromIntegral ds), SqlInteger (fromIntegral n) ]
  pure [ (sqlToInt eid, f)
       | [eid, f] <- rows
       , Just f   <- [sqlToMaybeDouble f] ]

-- | Non-dominated classes over (max fitness, min dl) on the dataset. Returns
-- the (eid, fitness, dl) triples that are not dominated by any other class.
pareto :: SqlBackend db => db -> Int -> IO [(EClassId, Double, Double)]
pareto db ds = do
  rows <- queryDb db
    "SELECT eid, fitness, dl FROM dataset_fit \
    \WHERE dataset_id = ? AND fitness IS NOT NULL AND dl IS NOT NULL"
    [SqlInteger (fromIntegral ds)]
  let pts = [ (sqlToInt eid, f, d)
            | [eid, ff, dd] <- rows
            , Just f <- [sqlToMaybeDouble ff]
            , Just d <- [sqlToMaybeDouble dd] ]
      dominates (f1, d1) (f0, d0) = f1 >= f0 && d1 <= d0 && (f1 > f0 || d1 < d0)
      nonDominated (eid_, f, d) = not (any (\(q0, qf, qd) -> dominates (qf, qd) (f, d)) pts)
  pure [ p | p@(_, f, d) <- pts, nonDominated p ]

-- | Non-dominated classes over (max fitness, min size) on the dataset.
paretoBySize :: SqlBackend db => db -> Int -> IO [(EClassId, Double, Int)]
paretoBySize db ds = do
  rows <- queryDb db
    "SELECT eid, fitness, size FROM dataset_fit \
    \WHERE dataset_id = ? AND fitness IS NOT NULL"
    [SqlInteger (fromIntegral ds)]
  let pts = [ (sqlToInt eid, f, s)
            | [eid, ff, ss] <- rows
            , Just f <- [sqlToMaybeDouble ff]
            , let s = sqlToInt ss ]
      dominates (f1, s1) (f0, s0) = f1 >= f0 && s1 <= s0 && (f1 > f0 || s1 < s0)
      nonDominated (eid_, f, s) = not (any (\(q0, qf, qs) -> dominates (qf, qs) (f, s)) pts)
  pure [ p | p@(_, f, s) <- pts, nonDominated p ]

-- | Number of evaluated e-classes per model size (up to @maxSize@) on the
-- dataset.
distributionCounts :: SqlBackend db => db -> Int -> Int -> IO [(Int, Int)]
distributionCounts db ds maxSize = do
  rows <- queryDb db
    "SELECT size, COUNT(*) FROM dataset_fit \
    \WHERE dataset_id = ? AND fitness IS NOT NULL AND size <= ? \
    \GROUP BY size ORDER BY size"
    [ SqlInteger (fromIntegral ds), SqlInteger (fromIntegral maxSize) ]
  pure [ (sqlToInt s, sqlToInt c) | [s, c] <- rows ]

-- | The e-class a previously-indexed expression maps to (NULL if never seen).
expressionEclass :: SqlBackend db => db -> Text -> IO (Maybe EClassId)
expressionEclass db key = do
  rows <- queryDb db "SELECT eclass FROM expression_index WHERE expression_key = ?"
                    [SqlText key]
  pure $ case rows of
    ([e] : _) -> Just (sqlToInt e)
    _         -> Nothing

-- | Whether a class has a fitness (i.e. was evaluated/fitted) on a dataset.
testedOnDataset :: SqlBackend db => db -> Int -> EClassId -> IO Bool
testedOnDataset db ds eid = do
  rows <- queryDb db
    "SELECT 1 FROM dataset_fit WHERE dataset_id = ? AND eid = ? AND fitness IS NOT NULL"
    [SqlInteger (fromIntegral ds), SqlInteger (fromIntegral eid)]
  pure (not (null rows))

-- | The e-node content keys that make up an e-class (multiple versions of one
-- expression).
versionsOf :: SqlBackend db => db -> EClassId -> IO [Text]
versionsOf db eid = do
  rows <- queryDb db "SELECT enode_key FROM eclass_node WHERE eid = ?"
                    [SqlInteger (fromIntegral eid)]
  pure [ sqlToText k | [k] <- rows ]

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