{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Out-of-core seed import: build an srtree e-graph directly in the database
-- by streaming a list of expressions into the relational schema.
--
-- The import holds **no** graph-size data in RAM: e-nodes are content-addressed
-- against the @enode@/@eclass_node@ tables (the @enode_key@ is their content
-- address), child lookups for n-ary flattening read each child's node from the
-- DB, and parent edges are written straight to the @parent@ table. Only a
-- scalar next-id counter is kept in memory, so peak memory is bounded (one
-- e-class at a time) regardless of how many expressions are imported.
--
-- Class pages (@cstore_page@) are written at the end in a single linear pass by
-- reconstructing each class from the relational tables, so hot classes are
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

import Control.Monad (forM, forM_, foldM)
import Control.Exception (SomeException, catch, displayException, try)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef', writeIORef)
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Text as T

import qualified Data.IntMap as IntMap
import qualified Data.HashSet as HashSet

import Data.Binary (encode)
import qualified Data.ByteString.Lazy as BL

import Data.SRTree
import Data.SRTree.Eval (Target)

import Algorithm.EqSat.Egraph
  ( EClassId, ENode(..), NOp(..), EClass(..), EClassData(..), Consts(..) )
import Algorithm.EqSat.Storage.Backend
  ( SqlValue(..), SqlBackend(..), sqlToInt, sqlToMaybeDouble, sqlToText )
import Algorithm.EqSat.Storage.ClassStore (classStoreTable, hex)
import Algorithm.EqSat.Storage.Types
  ( enodeKey, enodeOpTag, enodeOpDetail, serializeTheta, parseTheta, parseEnodeKey )
import Algorithm.EqSat.Storage.Schema (createSchema)
import Algorithm.EqSat.Storage.Query (getOrCreateDataset, writeDatasetFit)

-- | Result of an out-of-core import.
data ImportSummary = ImportSummary
  { isNextId :: !Int      -- ^ next free e-class id
  , isClasses :: !Int     -- ^ total e-classes written
  , isExpressions :: !Int -- ^ root expressions inserted
  } deriving (Show, Eq)

-- | The only in-process state: the next free e-class id (a single scalar, so
-- import memory is independent of the number of classes).
data ImportState = ImportState
  { stNextId :: !Int
  }

-- | Insert a list of @(expression, theta, fitness)@ into the database,
-- structurally expanding every subexpression into its own e-class, then write
-- the class pages in a final linear pass. Runs inside a single transaction
-- (rolled back on error).
importEqs :: SqlBackend db => db -> String -> [(Fix SRTree, [Target], Maybe Double)] -> IO (Either String ImportSummary)
importEqs db ds eqs = do
  createSchema db
  -- content-address dedup queries by enode_key, but eclass_node's PK is
  -- (eid, enode_key); index enode_key so those lookups are O(log n) not a scan.
  execDb db "CREATE INDEX IF NOT EXISTS idx_eclass_node_enode_key ON eclass_node(enode_key)"
  dsid <- getOrCreateDataset db ds
  ref <- newIORef (ImportState 0)
  r <- try $ do
    execDb db "BEGIN"
    -- stream the expression list through a fold (rather than forM_ + length
    -- eqs) so the lazy list is unreferenced after consumption and GC'd as it
    -- is processed; the fold accumulator carries the count, so we never retain
    -- the whole parsed expression list in memory.
    n <- foldM (\c (t, theta, fit) -> do
                   (eid, h) <- insertTree ref db fit dsid t
                   insertFit db eid theta fit h
                   writeDatasetFit db dsid eid fit Nothing (T.pack (serializeTheta theta)) h
                   -- record the root expression in the registry (keyed by its
                   -- canonical root node) so "was this expression seen/tested?"
                   -- can be answered per dataset.
                   mroot <- lookupClassNode db eid
                   forM_ mroot $ \en ->
                     runDb db
                       "INSERT OR REPLACE INTO expression_index (expression_key, eclass, dataset_id) VALUES (?, ?, ?)"
                       [ SqlText (T.pack (enodeKey en))
                       , SqlInteger (fromIntegral eid)
                       , SqlInteger (fromIntegral dsid) ]
                   pure (c + 1)) 0 eqs
    writeMeta db ref
    writeAllPages db
    execDb db "COMMIT"
    pure n
  case r of
    Left (e :: SomeException) -> do
      _ <- (execDb db "ROLLBACK" `catch` \(_ :: SomeException) -> pure ())
      pure (Left ("importEqs failed: " <> displayException e))
    Right n -> do
      st <- readIORef ref
      pure (Right (ImportSummary (stNextId st) (stNextId st) n))

-- | Insert a full tree bottom-up, returning its root e-class id and height.
insertTree :: SqlBackend db => IORef ImportState -> db -> Maybe Double -> Int -> Fix SRTree -> IO (EClassId, Int)
insertTree ref db fit dsid t = case unfix t of
  Var ix     -> insertNode ref db fit dsid (EVar ix) []
  Param ix   -> insertNode ref db fit dsid (EParam ix) []
  Const x    -> insertNode ref db fit dsid (EConst x) []
  Uni f sub  -> do
    (c, ch) <- insertTree ref db fit dsid sub
    insertNode ref db fit dsid (EUni f c) [(c, 1, ch)]
  Bin Add l r -> insertNAry ref db fit dsid EAdd l r
  Bin Mul l r -> insertNAry ref db fit dsid EMul l r
  Bin op l r  -> do
    (lc, lh) <- insertTree ref db fit dsid l
    (rc, rh) <- insertTree ref db fit dsid r
    insertNode ref db fit dsid (EBin op lc rc) [(lc, 1, lh), (rc, 1, rh)]

-- | Insert a flattened n-ary node (@Add@/@Mul@), merging nested same-op chains
-- the same way 'mkENaryM' does (a child whose class holds a single same-op
-- ENAry is flattened in). Children and their heights are read from the DB.
insertNAry :: SqlBackend db => IORef ImportState -> db -> Maybe Double -> Int -> NOp -> Fix SRTree -> Fix SRTree -> IO (EClassId, Int)
insertNAry ref db fit dsid op l r = do
  (c1, _) <- insertTree ref db fit dsid l
  (c2, _) <- insertTree ref db fit dsid r
  flat <- flattenChildren db op [(c1, 1), (c2, 1)]
  childsH <- forM (IntMap.toList flat) $ \(c, n) -> do
    h <- classHeight db c
    pure (c, n, h)
  insertNode ref db fit dsid (ENAry op flat) childsH

-- | Flatten @n@ occurrences of @cid@ when its class holds exactly one ENAry of
-- the same op (scaled by @n@); otherwise keep @cid@. Each child's node is read
-- from the database (no in-memory class index).
flattenChildren :: SqlBackend db => db -> NOp -> [(EClassId, Int)] -> IO (IntMap.IntMap Int)
flattenChildren db op children = do
  ms <- forM children $ \(c, n) -> do
    men <- lookupClassNode db c
    case men of
      Just (ENAry op' m') | op' == op -> pure (IntMap.map (* n) m')
      _                               -> pure (IntMap.singleton c n)
  pure (IntMap.unionsWith (+) ms)

-- | Content-addressed insert of a single e-node. Returns the e-class id and
-- height of the node (reusing the existing class when already present).
insertNode :: SqlBackend db => IORef ImportState -> db -> Maybe Double -> Int -> ENode -> [(EClassId, Int, Int)] -> IO (EClassId, Int)
insertNode ref db fit dsid en children = do
  let key   = enodeKey en
      childs = dedupChildren children
  mEid <- lookupEnodeId db key
  case mEid of
    Just eid -> do
      h <- classHeight db eid
      pure (eid, h)
    Nothing -> do
      st <- readIORef ref
      let eid = stNextId st
          h   = 1 + maximum (0 : [ ch | (_, _, ch) <- childs ])
      writeIORef ref (st { stNextId = eid + 1 })
      writeNode db eid en key childs h fit dsid
      pure (eid, h)

-- | Merge duplicate child e-classes into multiplicities (e.g. @x0 - x0@ has
-- both children in the same class), keeping the max height.
dedupChildren :: [(EClassId, Int, Int)] -> [(EClassId, Int, Int)]
dedupChildren =
  map (\(c, (n, h)) -> (c, n, h)) . IntMap.toList
  . IntMap.fromListWith (\(n1, h1) (n2, h2) -> (n1 + n2, max h1 h2))
  . map (\(c, n, h) -> (c, (n, h)))

-- | Write the relational rows for a brand-new e-node. Reverse parent edges go
-- straight to the @parent@ table (reconstructed into pages by 'writeAllPages').
writeNode :: SqlBackend db => db -> EClassId -> ENode -> String -> [(EClassId, Int, Int)] -> Int -> Maybe Double -> Int -> IO ()
writeNode db eid en key children h fit dsid = do
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
  forM_ children $ \(c, _, _) ->
    runDb db "INSERT INTO parent (child_eid, parent_eid, parent_enode_key) VALUES (?, ?, ?)"
      [ SqlInteger (fromIntegral c)
      , SqlInteger (fromIntegral eid)
      , SqlText (T.pack key) ]
  -- every class gets a fit row (legacy) and a dataset_fit row so the graph is
  -- fitness-annotated per dataset; the root's proper params/theta are written
  -- by the caller.
  insertFit db eid [] fit h
  writeDatasetFit db dsid eid fit Nothing "" h

-- | ENAry children as (class, multiplicity); empty for all other node shapes
-- (their children live inline in the content key).
naryChildrenOf :: ENode -> [(EClassId, Int)]
naryChildrenOf (ENAry _ m) = IntMap.toList m
naryChildrenOf _           = []

-- | Look up an existing e-class id for a content key (NULL if absent).
lookupEnodeId :: SqlBackend db => db -> String -> IO (Maybe EClassId)
lookupEnodeId db key = do
  rows <- queryDb db "SELECT eid FROM eclass_node WHERE enode_key = ?"
                    [SqlText (T.pack key)]
  pure $ case rows of
    ([eid] : _) -> Just (sqlToInt eid)
    _           -> Nothing

-- | Height of an e-class (from the @eclass@ row).
classHeight :: SqlBackend db => db -> EClassId -> IO Int
classHeight db eid = do
  rows <- queryDb db "SELECT height FROM eclass WHERE eid = ?"
                    [SqlInteger (fromIntegral eid)]
  pure $ case rows of
    ([h] : _) -> sqlToInt h
    _         -> 0

-- | The singleton e-node of a class (NULL if the class has no node row yet).
lookupClassNode :: SqlBackend db => db -> EClassId -> IO (Maybe ENode)
lookupClassNode db eid = do
  rows <- queryDb db "SELECT enode_key FROM eclass_node WHERE eid = ?"
                    [SqlInteger (fromIntegral eid)]
  pure $ case rows of
    ([SqlText k] : _) -> parseEnodeKey (T.unpack k)
    _                 -> Nothing

-- | Write every class page once, reconstructing each class from the relational
-- tables (node + height from @eclass_node@/@eclass@, parents from @parent@,
-- metrics from @fit@). Memory stays bounded to a single class at a time.
writeAllPages :: SqlBackend db => db -> IO ()
writeAllPages db = do
  cids <- queryDb db "SELECT eid FROM eclass ORDER BY eid" []
  forM_ cids $ \[eidCol] -> do
    let eid = sqlToInt eidCol
    nh <- queryDb db
      "SELECT n.enode_key, c.height FROM eclass_node n \
      \JOIN eclass c ON c.eid = n.eid WHERE n.eid = ?"
      [SqlInteger (fromIntegral eid)]
    case nh of
      ([SqlText k, hcol] : _) -> do
        let en = fromMaybe (error ("importEqs: bad node key for eid " <> show eid))
                           (parseEnodeKey (T.unpack k))
            h  = sqlToInt hcol
        pr <- queryDb db "SELECT parent_eid, parent_enode_key FROM parent WHERE child_eid = ?"
                         [SqlInteger (fromIntegral eid)]
        let parents = HashSet.fromList
              [ (sqlToInt pe, fromMaybe (error "importEqs: bad parent key") (parseEnodeKey (T.unpack (sqlToText pk))))
              | [pe, pk] <- pr ]
        -- Pages are structural-only: cost/best are derived on load, and
        -- fitness/dl/theta are dataset metadata (dataset_fit), so they are NOT
        -- baked into the e-graph blob (keeps the graph reusable across datasets).
        writeClassPage db eid (EClass eid (HashSet.singleton en) parents h (defaultInfo en h))
      _ -> pure ()

-- | Serialize an e-class to the page store (INSERT OR REPLACE so the final
-- pass is idempotent).
writeClassPage :: SqlBackend db => db -> EClassId -> EClass -> IO ()
writeClassPage db eid ec =
  runDb db ("INSERT OR REPLACE INTO " <> classStoreTable <> " (key, blob) VALUES (?, ?)")
    [ SqlInteger (fromIntegral eid)
    , SqlText (hex (BL.toStrict (encode ec))) ]

-- | Per-class data. Cost/best are derived quantities recomputed on load
-- ('recalculateBestAll'). Fitness/dl/theta/size are baked into the page (so
-- out-of-core reads via the paged store see them). @_consts@ is set from the
-- node so constant-folding rules behave identically to the in-memory seed.
defaultInfo :: ENode -> Int -> EClassData
defaultInfo en h = EData 0 en (constOf en) Nothing Nothing [] h

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
