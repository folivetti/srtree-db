{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Out-of-core seed import: build an srtree e-graph directly in the database
-- by streaming a list of expressions into the relational schema.
--
-- The import inserts structure only (content-addressed hash-consing): every
-- subexpression is expanded into its own e-class, exactly as the in-memory
-- 'fromTree' does, but identical e-nodes merge via the content-addressable
-- @enode_key@ instead of an in-RAM @nodeToEClass@ map of full class bodies.
-- No class bodies, cost/best data or derived range DBs are ever built, so peak
-- memory stays proportional to the *skeleton* (one node + parent edges per
-- class) rather than the multi-GB fully-materialized graph.
--
-- Class pages (@cstore_page@) are written only at the end in a single linear
-- pass, so hot classes (e.g. a variable referenced by every expression) are
-- never rewritten repeatedly.
--
-- The produced database is byte-compatible with 'saveGraph': the same page
-- blobs and relational rows, so 'loadGraphLazy' / 'dbEqSat' work on it
-- unchanged. Saturation is deliberately NOT performed here (structural-only);
-- rule rewrites are left to the out-of-core 'dbEqSat' path.
module Algorithm.EqSat.Storage.Import
  ( ImportSummary(..)
  , importEqs
  ) where

import Control.Monad (forM, forM_)
import Control.Exception (SomeException, catch, displayException, try)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef', writeIORef)
import Data.Maybe (catMaybes)
import qualified Data.Text as T

import qualified Data.IntMap as IntMap
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet

import Data.Binary (encode)
import qualified Data.ByteString.Lazy as BL

import Data.SRTree
import Data.SRTree.Eval (Target)

import Algorithm.EqSat.Egraph
  ( EClassId, ENode(..), NOp(..), EClass(..), EClassData(..), Consts(..) )
import Algorithm.EqSat.Storage.Backend
  ( SqlValue(..), SqlBackend(..) )
import Algorithm.EqSat.Storage.ClassStore (classStoreTable, hex)
import Algorithm.EqSat.Storage.Types (enodeKey, enodeOpTag, enodeOpDetail, serializeTheta)
import Algorithm.EqSat.Storage.Schema (createSchema)

-- | Result of an out-of-core import.
data ImportSummary = ImportSummary
  { isNextId :: !Int      -- ^ next free e-class id
  , isClasses :: !Int     -- ^ total e-classes written
  , isExpressions :: !Int -- ^ root expressions inserted
  } deriving (Show, Eq)

-- | In-process import state: the class skeleton (content key -> id, id -> node
-- and height) plus the accumulated reverse-parent edges.
data ImportState = ImportState
  { stNextId :: !Int
  , stByKey  :: !(HashMap.HashMap String EClassId)
  , stClass  :: !(HashMap.HashMap EClassId (ENode, Int, Maybe Double))
  , stParents :: !(HashMap.HashMap EClassId (HashSet.HashSet (EClassId, ENode)))
  }

-- | Insert a list of @(expression, theta, fitness)@ into the database,
-- structurally expanding every subexpression into its own e-class, then write
-- the class pages in a final linear pass. Runs inside transactions (rolled
-- back on error).
importEqs :: SqlBackend db => db -> [(Fix SRTree, [Target], Maybe Double)] -> IO (Either String ImportSummary)
importEqs db eqs = do
  createSchema db
  ref <- newIORef (ImportState 0 HashMap.empty HashMap.empty HashMap.empty)
  r <- try $ do
    execDb db "BEGIN"
    forM_ eqs $ \(t, theta, fit) -> do
      (eid, h) <- insertTree ref db fit t
      -- the root's params/theta are only meaningful at the root; overwrite the
      -- fit row written during insertion (which used empty theta).
      insertFit db eid theta fit h
    writeMeta db ref
    execDb db "COMMIT"
  case r of
    Left (e :: SomeException) -> do
      _ <- (execDb db "ROLLBACK" `catch` \(_ :: SomeException) -> pure ())
      pure (Left ("importEqs failed: " <> displayException e))
    Right () -> do
      st <- readIORef ref
      rp <- try (writeAllPages db st)
      case rp of
        Left (e :: SomeException) ->
          pure (Left ("importEqs (pages) failed: " <> displayException e))
        Right () ->
          pure (Right (ImportSummary (stNextId st) (stNextId st) (length eqs)))

-- | Write every class page once, from the in-memory skeleton.
writeAllPages :: SqlBackend db => db -> ImportState -> IO ()
writeAllPages db st = do
  execDb db "BEGIN"
  mapM_ (uncurry (writeClassPage db))
    [ (eid, EClass eid (HashSet.singleton en)
           (HashMap.lookupDefault HashSet.empty eid (stParents st))
           h (defaultInfo en h fit))
    | (eid, (en, h, fit)) <- HashMap.toList (stClass st) ]
  execDb db "COMMIT"

-- | Insert a full tree bottom-up, returning its root e-class id and height.
insertTree :: SqlBackend db => IORef ImportState -> db -> Maybe Double -> Fix SRTree -> IO (EClassId, Int)
insertTree ref db fit t = case unfix t of
  Var ix     -> insertNode ref db fit (EVar ix) []
  Param ix   -> insertNode ref db fit (EParam ix) []
  Const x    -> insertNode ref db fit (EConst x) []
  Uni f sub  -> do
    (c, _) <- insertTree ref db fit sub
    insertNode ref db fit (EUni f c) [(c, 1)]
  Bin Add l r -> insertNAry ref db fit EAdd l r
  Bin Mul l r -> insertNAry ref db fit EMul l r
  Bin op l r  -> do
    (lc, _) <- insertTree ref db fit l
    (rc, _) <- insertTree ref db fit r
    insertNode ref db fit (EBin op lc rc) [(lc, 1), (rc, 1)]

-- | Insert a flattened n-ary node (@Add@/@Mul@), merging nested same-op chains
-- the same way 'mkENaryM' does (a child whose class holds a single same-op
-- ENAry is flattened in).
insertNAry :: SqlBackend db => IORef ImportState -> db -> Maybe Double -> NOp -> Fix SRTree -> Fix SRTree -> IO (EClassId, Int)
insertNAry ref db fit op l r = do
  (c1, _) <- insertTree ref db fit l
  (c2, _) <- insertTree ref db fit r
  flat <- flattenChildren ref op [(c1, 1), (c2, 1)]
  insertNode ref db fit (ENAry op flat) (IntMap.toList flat)

-- | Flatten @n@ occurrences of @cid@ when its class holds exactly one ENAry of
-- the same op (scaled by @n@); otherwise keep @cid@.
flattenChildren :: IORef ImportState -> NOp -> [(EClassId, Int)] -> IO (IntMap.IntMap Int)
flattenChildren ref op children = do
  st <- readIORef ref
  let ms = [ case HashMap.lookup c (stClass st) of
               Just (ENAry op' m', _, _) | op' == op -> IntMap.map (* n) m'
               _                                     -> IntMap.singleton c n
           | (c, n) <- children ]
  pure (IntMap.unionsWith (+) ms)

-- | Content-addressed insert of a single e-node. Returns the e-class id and
-- height of the node (reusing the existing class when already present).
insertNode :: SqlBackend db => IORef ImportState -> db -> Maybe Double -> ENode -> [(EClassId, Int)] -> IO (EClassId, Int)
insertNode ref db fit en children = do
  let key   = enodeKey en
      childs = IntMap.toList (IntMap.fromListWith (+) children)
  st <- readIORef ref
  case HashMap.lookup key (stByKey st) of
    Just eid -> do
      let (_, h, _) = HashMap.lookupDefault (en, 0, Nothing) eid (stClass st)
      pure (eid, h)
    Nothing -> do
      let eid    = stNextId st
          h      = 1 + maximum (0 : [ maybe 0 (\(_, h, _) -> h) (HashMap.lookup c (stClass st)) | (c, _) <- childs ])
          st1    = st { stNextId = eid + 1
                      , stByKey  = HashMap.insert key eid (stByKey st)
                      , stClass  = HashMap.insert eid (en, h, fit) (stClass st)
                      , stParents = HashMap.unionWith HashSet.union
                          (stParents st)
                          (HashMap.fromList
                            [ (c, HashSet.singleton (eid, en)) | (c, _) <- childs ])
                      }
      writeIORef ref st1
      writeNode db eid en key childs h fit
      pure (eid, h)

-- | Write the relational rows for a brand-new e-node. Reverse parent edges are
-- recorded on the children's accumulated parent sets in memory (their pages are
-- written later, once, by 'writeAllPages').
writeNode :: SqlBackend db => db -> EClassId -> ENode -> String -> [(EClassId, Int)] -> Int -> Maybe Double -> IO ()
writeNode db eid en key children h fit = do
  runDb db "INSERT INTO enode (key, op, op_detail) VALUES (?, ?, ?)"
    [ SqlText (T.pack key)
    , SqlText (T.pack (enodeOpTag en))
    , SqlText (T.pack (enodeOpDetail en)) ]
  runDb db "INSERT INTO eclass (eid, canonical, height) VALUES (?, ?, ?)"
    [ SqlInteger (fromIntegral eid)
    , SqlInteger (fromIntegral eid)
    , SqlInteger (fromIntegral h) ]
  runDb db "INSERT INTO eclass_node (eid, enode_key) VALUES (?, ?)"
    [ SqlInteger (fromIntegral eid), SqlText (T.pack key) ]
  -- enode_child rows exist only for ENAry nodes (EBin/Uni children live in the
  -- content key); the multiset is unique.
  forM_ (naryChildrenOf en) $ \(c, n) ->
    runDb db "INSERT INTO enode_child (enode_key, child_eid, cnt) VALUES (?, ?, ?)"
      [ SqlText (T.pack key)
      , SqlInteger (fromIntegral c)
      , SqlInteger (fromIntegral n) ]
  forM_ children $ \(c, _) ->
    runDb db "INSERT INTO parent (child_eid, parent_eid, parent_enode_key) VALUES (?, ?, ?)"
      [ SqlInteger (fromIntegral c)
      , SqlInteger (fromIntegral eid)
      , SqlText (T.pack key) ]
  -- every class gets a fit row so the graph is fitness-annotated (dbTop ranks
  -- by it); the root's proper params/theta are written by the caller.
  insertFit db eid [] fit h

-- | ENAry children as (class, multiplicity); empty for all other node shapes
-- (their children live inline in the content key).
naryChildrenOf :: ENode -> [(EClassId, Int)]
naryChildrenOf (ENAry _ m) = IntMap.toList m
naryChildrenOf _           = []

-- | Serialize an e-class to the page store (INSERT OR REPLACE so the final
-- pass is idempotent).
writeClassPage :: SqlBackend db => db -> EClassId -> EClass -> IO ()
writeClassPage db eid ec =
  runDb db ("INSERT OR REPLACE INTO " <> classStoreTable <> " (key, blob) VALUES (?, ?)")
    [ SqlInteger (fromIntegral eid)
    , SqlText (hex (BL.toStrict (encode ec))) ]

-- | Per-class data. Cost/best are derived quantities recomputed on load
-- ('recalculateBestAll'). Fitness is baked into the page (so out-of-core reads
-- via the paged store see it); dl/theta/size are left to the @fit@ table.
-- @_consts@ is set from the node so constant-folding rules behave identically
-- to the in-memory seed (a class holding a literal constant must report it, or
-- 'foldConsts' creates spurious classes during eqsat).
defaultInfo :: ENode -> Int -> Maybe Double -> EClassData
defaultInfo en h fit = EData 0 en (constOf en) fit Nothing [] h

constOf :: ENode -> Consts
constOf (EConst x)   = ConstVal x
constOf (EParam ix)  = ParamIx ix
constOf _            = NotConst

-- | Write a @fit@ row for a root e-class. NULL fitness is stored as literal NULL.
insertFit :: SqlBackend db => db -> EClassId -> [Target] -> Maybe Double -> Int -> IO ()
insertFit db eid theta fit sz = do
  let (fitCol, fitVal) = case fit of
        Nothing -> ("NULL", Nothing)
        Just f  -> ("?", Just (SqlFloat f))
  runDb db ("INSERT OR REPLACE INTO fit (eid, fitness, dl, theta, size) VALUES (?, "
             <> fitCol <> ", NULL, ?, ?)")
    (catMaybes [ Just (SqlInteger (fromIntegral eid))
               , fitVal
               , Just (SqlText (T.pack (serializeTheta theta)))
               , Just (SqlInteger (fromIntegral sz)) ])

-- | Write the @meta@ scalars: next free id and DB-tracking flag (mirrors the
-- in-memory seed graph, which runs with range-DB tracking enabled).
writeMeta :: SqlBackend db => db -> IORef ImportState -> IO ()
writeMeta db ref = do
  st <- readIORef ref
  runDb db "INSERT INTO meta (key, value) VALUES (?, ?)"
    [ SqlText "next_id", SqlText (T.pack (show (stNextId st))) ]
  runDb db "INSERT INTO meta (key, value) VALUES (?, ?)"
    [ SqlText "track_dbs", SqlText "1" ]