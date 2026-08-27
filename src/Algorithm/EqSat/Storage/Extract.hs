{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

-- | Standalone SRTree reconstruction from the relational DB, without loading
-- the full e-graph. Walks @cstore_page@ blobs one class at a time, resolving
-- children recursively. Memory is O(depth) — no page cache, no in-memory maps.
module Algorithm.EqSat.Storage.Extract
  ( extractTreeFromDB
  , readPage
  , reconstructFromCache
  , expandTreeIds
  ) where

import Data.Binary (decode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.IntMap as IntMap
import qualified Data.HashSet as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Data.SRTree (Fix(..), SRTree(..), Op(..), Function(..))
import Algorithm.EqSat.Egraph (EClassId, EClass(..), ENode(..), NOp(..), toOp)
import Algorithm.EqSat.Storage.Backend
  ( SqlBackend(..), SqlValue(..), sqlToInt, sqlToBlob )
import Algorithm.EqSat.Storage.ClassStore (classStoreTable)

-- | Reconstruct a 'Fix SRTree' for the given e-class by walking the page
-- store and relational tables directly. No 'EGraph' state is needed.
--
-- Returns 'Nothing' if the e-class has no page blob or the tree exceeds the
-- expansion budget (200 nodes, same as 'getBestExprBounded').
extractTreeFromDB :: SqlBackend db => db -> EClassId -> IO (Maybe (Fix SRTree))
extractTreeFromDB db root = go IntSet.empty 0 root
  where
    budget :: Int
    budget = 200

    go :: IntSet -> Int -> EClassId -> IO (Maybe (Fix SRTree))
    go _ n _ | n >= budget = pure Nothing
    go seen n eid
      | IntSet.member eid seen = pure Nothing
      | otherwise = do
          mPage <- readPage db eid
          case mPage of
            Nothing -> pure Nothing
            Just page -> do
              let ec = decode page :: EClass
                  nodes = Set.toList (_eNodes ec)
              case nodes of
                [] -> pure Nothing
                (en : _) -> expandNode (IntSet.insert eid seen) n en

    expandNode :: IntSet -> Int -> ENode -> IO (Maybe (Fix SRTree))
    expandNode _ _ (EVar ix)   = pure (Just (Fix (Var ix)))
    expandNode _ _ (EParam ix) = pure (Just (Fix (Param ix)))
    expandNode _ _ (EConst x)  = pure (Just (Fix (Const x)))
    expandNode seen n (EUni f t) = do
      mt <- go seen (n + 1) t
      case mt of
        Nothing -> pure Nothing
        Just t' -> pure (Just (Fix (Uni f t')))
    expandNode seen n (EBin op l r) = do
      ml <- go seen (n + 1) l
      case ml of
        Nothing -> pure Nothing
        Just l' -> do
          mr <- go seen (n + 1) r
          case mr of
            Nothing -> pure Nothing
            Just r' -> pure (Just (Fix (Bin op l' r')))
    expandNode seen n (ENAry op m) = do
      let children = IntMap.toAscList m
      mts <- expandNary seen n children
      pure $ naryTree op <$> mts

    -- Expand each child in the ENAry multiset, collecting results.
    -- Each child is expanded once, then replicated by its multiplicity.
    expandNary :: IntSet -> Int -> [(EClassId, Int)] -> IO (Maybe [Fix SRTree])
    expandNary _ _ [] = pure (Just [])
    expandNary seen n ((cid, cnt) : rest) = do
      mc <- go seen (n + 1) cid
      case mc of
        Nothing -> pure Nothing
        Just c  -> do
          mrest <- expandNary seen (n + 1) rest
          case mrest of
            Nothing -> pure Nothing
            Just rs -> pure (Just (replicate (min cnt (budget - n)) c ++ rs))

    -- Right-fold a list of child expressions into a binary Fix SRTree,
    -- then normalize Sub/Div (same as Egraph.naryTree).
    naryTree :: NOp -> [Fix SRTree] -> Fix SRTree
    naryTree _ [] = Fix (Var 0)
    naryTree op ts = normalizeSubDiv (foldr1 (\a b -> Fix (Bin (toOp op) a b)) ts)

    normalizeSubDiv :: Fix SRTree -> Fix SRTree
    normalizeSubDiv (Fix (Bin Add l r)) = case pick l r of
        Just (pos, neg) -> Fix (Bin Sub pos neg)
        Nothing         -> Fix (Bin Add (normalizeSubDiv l) (normalizeSubDiv r))
      where
        pick a b = case negated a of
                     Just t -> Just (b, t)
                     Nothing -> case negated b of
                                  Just t -> Just (a, t)
                                  Nothing -> Nothing
        negated (Fix (Bin Mul (Fix (Const c)) t)) | c == -1 = Just t
        negated (Fix (Bin Mul t (Fix (Const c)))) | c == -1 = Just t
        negated (Fix (Const c)) | c < 0 = Just (Fix (Const (-c)))
        negated _ = Nothing
    normalizeSubDiv (Fix (Bin Mul l r)) = case pick l r of
        Just (num, den) -> Fix (Bin Div num den)
        Nothing         -> Fix (Bin Mul (normalizeSubDiv l) (normalizeSubDiv r))
      where
        pick a b = case a of
                     Fix (Uni Recip t) -> Just (b, t)
                     _ -> case b of
                            Fix (Uni Recip t) -> Just (a, t)
                            _ -> Nothing
    normalizeSubDiv (Fix (Uni f t)) = Fix (Uni f (normalizeSubDiv t))
    normalizeSubDiv t = t

-- | Read a single page blob for an e-class (raw binary, no decoding).
readPage :: SqlBackend db => db -> EClassId -> IO (Maybe BL.ByteString)
readPage db eid = do
  rows <- queryDb db
    ("SELECT blob FROM " <> classStoreTable <> " WHERE key = ?")
    [SqlInteger (fromIntegral eid)]
  case rows of
    [[SqlBlob bs]] -> pure (Just (BL.fromStrict bs))
    [[SqlText hv]] -> pure (Just (BL.fromStrict (sqlToBlob (SqlText hv))))  -- backward compat
    _              -> pure Nothing

-- | Pure SRTree reconstruction from a pre-loaded IntMap cache.
-- No IO, no SQL — O(1) per node lookup.
--
-- Returns 'Nothing' if the e-class is not in the cache or the tree exceeds
-- the expansion budget (200 nodes).
reconstructFromCache :: IntMap.IntMap EClass -> EClassId -> Maybe (Fix SRTree)
reconstructFromCache cache root = go IntSet.empty 0 root
  where
    go seen n eid
      | n >= 200 = Nothing
      | IntSet.member eid seen = Nothing
      | otherwise = case IntMap.lookup eid cache of
          Nothing -> Nothing
          Just ec ->
            let nodes = Set.toList (_eNodes ec)
            in case nodes of
                 [] -> Nothing
                 (en : _) -> expandNode (IntSet.insert eid seen) n en

    expandNode _ _ (EVar ix)   = Just (Fix (Var ix))
    expandNode _ _ (EParam ix) = Just (Fix (Param ix))
    expandNode _ _ (EConst x)  = Just (Fix (Const x))
    expandNode seen n (EUni f t) = Fix . Uni f <$> go seen (n + 1) t
    expandNode seen n (EBin op l r) = do
      l' <- go seen (n + 1) l
      r' <- go seen (n + 1) r
      pure (Fix (Bin op l' r'))
    expandNode seen n (ENAry op m) = do
      let children = IntMap.toAscList m
      ts <- expandNary seen n op children
      pure (naryTree op ts)

    expandNary _ _ _ [] = Just []
    expandNary seen n op ((cid, cnt) : rest) = do
      c <- go seen (n + 1) cid
      rs <- expandNary seen (n + 1) op rest
      pure (replicate (min cnt (200 - n)) c ++ rs)

    naryTree _ [] = Fix (Var 0)
    naryTree op ts = normalizeSubDiv (foldr1 (\a b -> Fix (Bin (toOp op) a b)) ts)

    normalizeSubDiv (Fix (Bin Add l r)) = case pick l r of
        Just (pos, neg) -> Fix (Bin Sub pos neg)
        Nothing         -> Fix (Bin Add (normalizeSubDiv l) (normalizeSubDiv r))
      where
        pick a b = case negated a of
                     Just t -> Just (b, t)
                     Nothing -> case negated b of
                                  Just t -> Just (a, t)
                                  Nothing -> Nothing
        negated (Fix (Bin Mul (Fix (Const c)) t)) | c == -1 = Just t
        negated (Fix (Bin Mul t (Fix (Const c)))) | c == -1 = Just t
        negated (Fix (Const c)) | c < 0 = Just (Fix (Const (-c)))
        negated _ = Nothing
    normalizeSubDiv (Fix (Bin Mul l r)) = case pick l r of
        Just (num, den) -> Fix (Bin Div num den)
        Nothing         -> Fix (Bin Mul (normalizeSubDiv l) (normalizeSubDiv r))
      where
        pick a b = case a of
                     Fix (Uni Recip t) -> Just (b, t)
                     _ -> case b of
                            Fix (Uni Recip t) -> Just (a, t)
                            _ -> Nothing
    normalizeSubDiv (Fix (Uni f t)) = Fix (Uni f (normalizeSubDiv t))
    normalizeSubDiv t = t

-- | Collect all eclass IDs referenced by the tree rooted at @eid@,
-- including the root itself. Used to pre-expand dependencies for bulk loading.
expandTreeIds :: IntMap.IntMap EClass -> EClassId -> IntSet
expandTreeIds cache root = go IntSet.empty 0 root
  where
    go seen n eid
      | n >= 200 = seen
      | IntSet.member eid seen = seen
      | otherwise = case IntMap.lookup eid cache of
          Nothing -> seen
          Just ec ->
            let seen' = IntSet.insert eid seen
                nodes = Set.toList (_eNodes ec)
            in case nodes of
                 [] -> seen'
                 (en : _) -> expandNode seen' n en

    expandNode seen _ (EVar _)   = seen
    expandNode seen _ (EParam _) = seen
    expandNode seen _ (EConst _)  = seen
    expandNode seen n (EUni _ t) = go seen (n + 1) t
    expandNode seen n (EBin _ l r) =
      let !seen' = go seen (n + 1) l
      in go seen' (n + 1) r
    expandNode seen n (ENAry _ m) =
      foldl' (\s (cid, _) -> go s (n + 1) cid) seen (IntMap.toAscList m)
