{-# LANGUAGE OverloadedStrings #-}

-- | Proof-of-concept for an O(1)-memory matcher: instead of building the
-- in-RAM pattern trie (@_patDB@, O(nodes)), stream the @enode@/@eclass_node@
-- tables through a SQLite cursor and match each node as it is read. Peak memory
-- is O(budget) (the bounded result set), independent of graph size.
--
-- This validates the mechanism behind P2.3 (the SQL/streaming matcher rewrite)
-- before committing to replacing the trie. It is SQLite-specific (uses
-- 'Database.SQLite3' directly, since 'SqlBackend' returns whole grids).
module Algorithm.EqSat.Storage.Stream
  ( streamByOpCount
  , streamMatchNAry
  , streamRootsByOp
  ) where

import Database.SQLite3
  ( Database, SQLData(..), StepResult(..)
  , bind, columns, step, withStatement )
import qualified Data.Text as T
import qualified Data.IntSet as IntSet

import Algorithm.EqSat.Egraph (EClassId, ENode(..))
import Algorithm.EqSat.Storage.Types (parseEnodeKey)
import Data.Int (Int64)

-- | Stream the @enode@ table by @op_detail@ and count rows without accumulating
-- them (the O(1)-memory baseline for a streaming matcher).
streamByOpCount :: Database -> T.Text -> IO Int
streamByOpCount db op = withStatement db
  "SELECT e.key FROM enode e WHERE e.op_detail = ?" $ \stmt -> do
    bind stmt [SQLText op]
    go stmt 0
  where
    go stmt n = do
      r <- step stmt
      case r of
        Done -> pure n
        Row  -> go stmt (n + 1)

-- | Stream the @eclass_node JOIN enode@ for a given operator, reconstruct each node
-- from its content key, and collect at most @budget@ e-class ids that actually
-- contain a node of that operator. Memory is O(@budget@), not O(nodes).
streamMatchNAry :: Database -> T.Text -> Int -> IO [EClassId]
streamMatchNAry db opBudget budget = withStatement db
  "SELECT n.eid, n.enode_key FROM eclass_node n \
  \JOIN enode e ON e.key = n.enode_key WHERE e.op_detail = ?" $ \stmt -> do
    bind stmt [SQLText opBudget]
    go stmt budget []
  where
    go stmt budgetLeft acc
      | budgetLeft <= 0 = pure (reverse acc)
      | otherwise = do
          r <- step stmt
          case r of
            Done -> pure (reverse acc)
            Row  -> do
              cols <- columns stmt
              let eid  = case cols of (SQLInteger i : _) -> fromIntegral i; _ -> 0
                  key  = case cols of (_ : SQLText k : _) -> T.unpack k; _ -> ""
                  ok   = case parseEnodeKey key of
                           Just (ENAry _ _) -> True
                           Just _           -> True
                           Nothing          -> False
              if ok
                then go stmt (budgetLeft - 1) (eid : acc)
                else go stmt budgetLeft acc

-- | Stream the distinct e-class ids that contain a node of a given @op_detail@
-- through a SQLite cursor, stopping after @budget@ non-excluded rows. This is
-- the O(1)-memory candidate-root source for the streaming n-ary matcher: it
-- never materializes the whole (operator -> root set) index in RAM. Memory is
-- O(@budget@ + size of @exclude@).
streamRootsByOp :: Database -> T.Text -> Int -> [EClassId] -> IO [EClassId]
streamRootsByOp db opDetail budget exclude = withStatement db
  "SELECT DISTINCT n.eid FROM eclass_node n \
  \JOIN enode e ON e.key = n.enode_key WHERE e.op_detail = ?" $ \stmt -> do
    bind stmt [SQLText opDetail]
    let ex = IntSet.fromList exclude
    go stmt ex budget []
  where
    go stmt ex n acc
      | n <= 0 = pure (reverse acc)
      | otherwise = do
          r <- step stmt
          case r of
            Done -> pure (reverse acc)
            Row  -> do
              cols <- columns stmt
              let eid = case cols of (SQLInteger i : _) -> fromIntegral i; _ -> 0
              if IntSet.member eid ex
                then go stmt ex n acc
                else go stmt ex (n - 1) (eid : acc)
