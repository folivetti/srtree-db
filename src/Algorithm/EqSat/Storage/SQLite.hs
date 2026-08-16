{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | SQLite-backed persistence for srtree e-graphs.
--
-- 'saveGraph' serializes an in-memory e-graph into the normalized schema:
-- structure in @enode@/@enode_child@/@eclass@/@eclass_node@, per-class risk
-- metrics in @fit@, scalars in @meta@. 'loadGraph' reconstructs the e-graph
-- via 'Algorithm.EqSat.Store.importEGraph', recomputing parent pointers and
-- the derived range databases.
--
-- Cost / best / consts are NOT persisted: they are derived quantities, re-set
-- to defaults on load (queries, pattern matching and refitting do not need
-- them). ENAry children (eclass -> multiplicity) are stored in @enode_child@;
-- Uni/Bin children are embedded in the content key.
--
-- The module is written against 'Algorithm.EqSat.Storage.Backend' and is
-- driver-neutral: the same code drives the PostgreSQL backend
-- ('Algorithm.EqSat.Storage.Postgres').
module Algorithm.EqSat.Storage.SQLite
  ( saveGraph
  , loadGraph
  , loadGraphLazy
  , pushFit
  , refreshFitness
  , query
  , flushStore
  ) where

import Control.Monad (forM, forM_, when)
import Control.Exception (SomeException, catch, displayException)
import Control.Monad.Identity (runIdentity)
import Control.Monad.State.Strict (execStateT)
import Data.Int (Int64)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (foldl')
import qualified Data.IntSet as IntSet
import qualified Data.IntMap as IntMap
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as Set
import qualified Data.Map.Strict as Map
import qualified Data.Set as RangeSet
import Data.Binary (decode, encode)
import qualified Data.ByteString.Lazy as BL

import Database.SQLite3
  ( Database, SQLData(..), StepResult(..)
  , bind, columns, exec, step, withStatement )

import Data.SRTree.Eval (Target)
import Algorithm.EqSat.Egraph
  ( EGraph(..), EClassId, ENode(..), Consts(..)
  , EClassPageStore(..), EClass(..), EClassData(..)
  , EGraphDB(..), emptyDB, canonical, insertRange, eChildren, eOpKey )
import Algorithm.EqSat.Build (populate)
import Algorithm.EqSat.Info (insertFitness)
import Algorithm.EqSat.Store
  ( GraphRows(..), EClassRow(..), exportEGraph, importEGraph, rebuildDBs )
import Algorithm.EqSat.Storage.Backend
  ( SqlValue(..), SqlBackend(..), sqlToInt, sqlToMaybeDouble, sqlToText )
import Algorithm.EqSat.Storage.ClassStore
  ( classStoreTable, openClassStore, allPages, classStoreHandle, hex )
import Algorithm.EqSat.Storage.Query (readDatasetFit, writeDatasetFit)
import Algorithm.EqSat.Storage.Types
import Algorithm.EqSat.Storage.Schema (createSchema, schemaSQL)

-- | Default cache capacity (pages) for the lazily paged e-class store.
defaultClassCap :: Int
defaultClassCap = 50000

-- ---------------------------------------------------------------------------
-- SQLite driver instance

instance SqlBackend Database where
  execDb = exec
  runDb db sql params = withStatement db sql $ \stmt -> do
    bind stmt (map toSqlData params)
    _ <- step stmt
    pure ()
  queryDb db sql params = withStatement db sql $ \stmt -> do
    bind stmt (map toSqlData params)
    go stmt []
    where
      go stmt acc = do
        r <- step stmt
        case r of
          Done -> pure (reverse acc)
          Row  -> do
            cols <- columns stmt
            go stmt (map fromSqlData cols : acc)
  createSchemaDb db = mapM_ (exec db) schemaSQL

toSqlData :: SqlValue -> SQLData
toSqlData (SqlInteger n) = SQLInteger n
toSqlData (SqlFloat d)   = SQLFloat d
toSqlData (SqlText t)    = SQLText t
toSqlData SqlNull        = SQLNull

fromSqlData :: SQLData -> SqlValue
fromSqlData (SQLInteger n) = SqlInteger n
fromSqlData (SQLFloat d)   = SqlFloat d
fromSqlData (SQLText t)    = SqlText t
fromSqlData SQLNull        = SqlNull
fromSqlData _              = SqlNull

-- | Driver-neutral parameterized query (abstracts the concrete backend).
query :: SqlBackend db => db -> Text -> [SqlValue] -> IO [[SqlValue]]
query = queryDb

-- | Driver-neutral parameterized statement (abstracts the concrete backend).
run :: SqlBackend db => db -> Text -> [SqlValue] -> IO ()
run = runDb

-- ---------------------------------------------------------------------------
-- writing

-- | Persist the full e-graph (structure + risk metrics + e-class pages).
-- Replaces any previously stored graph in this database.
--
-- Every canonical e-class is written as a serialized page to @cstore_page@ in
-- addition to the normalized relational schema, so the graph round-trips
-- through the lazily paged 'loadGraph' as well as the relational path.
saveGraph :: SqlBackend db => db -> Int -> EGraph -> IO (Either String ())
saveGraph db dsid eg = do
  createSchema db
  let rows0 = exportEGraph eg
  gr <- graphClassRows eg
  let rows = rows0 { _grEClasses = gr }
  execDb db "BEGIN"
  result <- (do
      clearTables db
      writeMeta db rows
      writeNodes db rows
      writeClasses db rows
      writeParents db rows
      writeFit db rows
      writeDatasetFitRows db dsid rows
      writeClassPages db rows
      execDb db "COMMIT"
      pure (Right ()))
    `catch` \(e :: SomeException) -> do
      -- roll back so a partial write never leaves the connection mid-transaction
      execDb db "ROLLBACK"
      pure (Left ("saveGraph failed: " <> displayException e))
  pure result

-- | Enumerate every canonical e-class row of a graph. For a paged graph the
-- resident @_eClass@ is a bounded cache that may hold classes created or mutated
-- after the last flush, so it is the authoritative source for any class it
-- contains; the persisted pages supply the remainder (classes evicted from the
-- resident cache or never touched). The two are unioned with the resident rows
-- taking precedence, so edits made through either the IO or the pure instances
-- are never lost on 'saveGraph'. A fully resident graph reads the complete map
-- through 'exportEGraph'.
graphClassRows :: EGraph -> IO (IntMap.IntMap EClassRow)
graphClassRows eg = case _classStore eg of
  Nothing -> pure (_grEClasses (exportEGraph eg))
  Just h  -> do
    pages    <- cpsAll h
    let storeRows = IntMap.fromList [ mkRow ec | ec <- pages ]
        resident  = _grEClasses (exportEGraph eg)
    pure (resident `IntMap.union` storeRows)
  where
    mkRow ec = (_eClassId ec, EClassRow (_eNodes ec) (_parents ec) (_height ec) (_info ec))

clearTables :: SqlBackend db => db -> IO ()
clearTables db = do
  execDb db "DELETE FROM parent"
  execDb db "DELETE FROM meta"
  execDb db "DELETE FROM enode_child"
  execDb db "DELETE FROM eclass_node"
  execDb db "DELETE FROM enode"
  execDb db "DELETE FROM eclass"
  execDb db "DELETE FROM fit"
  execDb db ("DELETE FROM " <> classStoreTable)

-- | Serialize and store every canonical e-class as a page in the page store
-- table, inside the calling transaction (no nested BEGIN/COMMIT).
writeClassPages :: SqlBackend db => db -> GraphRows -> IO ()
writeClassPages db rows =
  forM_ (IntMap.toAscList (_grEClasses rows)) $ \(eid, r) ->
    run db ("INSERT INTO " <> classStoreTable <> " (key, blob) VALUES (?, ?)")
      [ SqlInteger (fromIntegral eid)
      , SqlText (hex (BL.toStrict (encode (EClass eid (_rcNodes r) (_rcParents r) (_rcHeight r) (_rcInfo r))))) ]

writeMeta :: SqlBackend db => db -> GraphRows -> IO ()
writeMeta db rows = do
  run db "INSERT INTO meta (key, value) VALUES (?, ?)"
    [ SqlText "next_id", SqlText (T.pack (show (_grNextId rows))) ]
  run db "INSERT INTO meta (key, value) VALUES (?, ?)"
    [ SqlText "track_dbs", SqlText (if _grTrackDBs rows then "1" else "0") ]

writeNodes :: SqlBackend db => db -> GraphRows -> IO ()
writeNodes db rows =
  forM_ (HashMap.toList (_grENodeToEClass rows)) $ \(en, eid) -> do
    let key = enodeKey en
    run db "INSERT INTO enode (key, op, op_detail) VALUES (?, ?, ?)"
      [ SqlText (T.pack key)
      , SqlText (T.pack (enodeOpTag en))
      , SqlText (T.pack (enodeOpDetail en)) ]
    run db "INSERT INTO eclass_node (eid, enode_key) VALUES (?, ?)"
      [ SqlInteger (fromIntegral eid), SqlText (T.pack key) ]
    forM_ (naryChildren en) $ \(c, n) ->
      run db "INSERT INTO enode_child (enode_key, child_eid, cnt) VALUES (?, ?, ?)"
        [ SqlText (T.pack key)
        , SqlInteger (fromIntegral c)
        , SqlInteger (fromIntegral n) ]

-- | Children of an ENAry node as (class, multiplicity); empty otherwise.
naryChildren :: ENode -> [(EClassId, Int)]
naryChildren (ENAry _ m) = IntMap.toList m
naryChildren _           = []

writeClasses :: SqlBackend db => db -> GraphRows -> IO ()
writeClasses db rows =
  forM_ (IntMap.toAscList (_grCanonical rows)) $ \(eid, canon) ->
    run db "INSERT INTO eclass (eid, canonical, height) VALUES (?, ?, ?)"
      [ SqlInteger (fromIntegral eid)
      , SqlInteger (fromIntegral canon)
      , SqlInteger (fromIntegral (maybe 0 _rcHeight (IntMap.lookup eid (_grEClasses rows)))) ]

-- | Persist the reverse edges: for every (parent class, parent e-node) in each
-- class's @_parents@, a @parent@ row keyed by the child e-class. This makes the
-- parent relation queryable per class without scanning @enode@/@eclass_node@.
writeParents :: SqlBackend db => db -> GraphRows -> IO ()
writeParents db rows =
  forM_ (IntMap.toAscList (_grEClasses rows)) $ \(eid, r) ->
    forM_ (Set.toList (_rcParents r)) $ \(pEid, pEn) ->
      run db "INSERT INTO parent (child_eid, parent_eid, parent_enode_key) VALUES (?, ?, ?)"
        [ SqlInteger (fromIntegral eid)
        , SqlInteger (fromIntegral pEid)
        , SqlText (T.pack (enodeKey pEn)) ]

-- | Insert a @fit@ row. NULL fitness/DL are expressed as literal @NULL@ (the
-- shared SQL is therefore driver-neutral).
-- | Write the legacy @fit@ rows (kept for the non-dataset 'loadGraph' test
-- loader; the dataset-aware path uses 'writeDatasetFitRows').
writeFit :: SqlBackend db => db -> GraphRows -> IO ()
writeFit db rows =
  forM_ (IntMap.toAscList (_grEClasses rows)) $ \(eid, r) -> do
    let info = _rcInfo r
        fit  = _fitness info
        dl   = _dl info
        sz   = _size info
        (fitCol, fitVal) = case fit of
                             Nothing -> ("NULL", Nothing)
                             Just f  -> ("?", Just (SqlFloat f))
        (dlCol, dlVal)   = case dl of
                             Nothing -> ("NULL", Nothing)
                             Just d  -> ("?", Just (SqlFloat d))
    run db ("INSERT INTO fit (eid, fitness, dl, theta, size) VALUES (?, "
              <> fitCol <> ", " <> dlCol <> ", ?, ?)")
      (catMaybes [ Just (SqlInteger (fromIntegral eid))
                 , fitVal
                 , dlVal
                 , Just (SqlText (T.pack (serializeTheta (_theta info))))
                 , Just (SqlInteger (fromIntegral sz)) ])

-- | Write the per-(dataset, e-class) fitness rows for every class in a graph.
writeDatasetFitRows :: SqlBackend db => db -> Int -> GraphRows -> IO ()
writeDatasetFitRows db dsid rows =
  forM_ (IntMap.toAscList (_grEClasses rows)) $ \(eid, r) -> do
    let info = _rcInfo r
    writeDatasetFit db dsid eid (_fitness info) (_dl info)
      (T.pack (serializeTheta (_theta info))) (_size info)

-- ---------------------------------------------------------------------------
-- reading

-- | Reconstruct the e-graph stored by a previous 'saveGraph'.
--
-- When the database carries e-class pages (@cstore_page@), the graph is
-- restored from those pages with an 'EClassPageStore' handle installed (so
-- subsequent mutations are written through to the store) and the relational
-- tables supply structure (canonical map, node -> class) and the @fit@ table
-- supplies the current risk metrics. Databases written without pages fall
-- back to the fully relational path.
loadGraph :: SqlBackend db => db -> IO (Either String EGraph)
loadGraph db = do
  m <- readMeta db
  case m of
    Nothing -> pure (Left "srtree-db: no e-graph stored in this database")
    Just (nextId, trackDBs) -> do
      enodes <- readNodes db
      ecLst  <- readClasses db
      fit    <- readFit db
      let canon     = IntMap.fromList [ (eid, c) | (eid, c, _) <- ecLst ]
          nodeToEClass = HashMap.fromList enodes
      ps <- openClassStore db defaultClassCap 1000
      pages <- allPages ps
      if null pages
        then do
          -- fully relational path (databases written before the page store)
          parents <- readParents db
          let storedParents = IntMap.fromListWith Set.union
                [ (c, Set.singleton (pEid, pEn))
                | (c, pEid, pEn) <- parents ]
              classes = buildClasses canon nodeToEClass storedParents (IntMap.fromList fit) (IntMap.fromList [ (eid, h) | (eid, _, h) <- ecLst ])
              rows    = GraphRows canon nodeToEClass classes nextId trackDBs
          pure (importEGraph rows)
        else do
          -- paged path: classes come from the serialized pages; the @fit@
          -- table overrides their risk metrics (it is the current DB truth).
          let fitMap = IntMap.fromList fit
              applyFit eid ec =
                case IntMap.lookup eid fitMap of
                  Nothing -> ec
                  Just (f, d, s, th) ->
                    ec { _info = (_info ec){ _fitness = f, _dl = d, _size = s, _theta = th } }
              classes  = IntMap.mapWithKey applyFit
                           (IntMap.fromList [ (eid, decode (BL.fromStrict page)) | (eid, page) <- pages ])
              toRow eid ec = EClassRow (_eNodes ec) (_parents ec) (_height ec) (_info ec)
              rows = GraphRows canon nodeToEClass (IntMap.mapWithKey toRow classes) nextId trackDBs
          case importEGraph rows of
            Left err  -> pure (Left err)
            Right eg  -> pure (Right eg { _classStore = Just (classStoreHandle ps) })

-- | Write back any pending dirty e-class pages when the graph carries a
-- paged store (a no-op on a fully resident graph). Call this at durable
-- commit points (e.g. rewrite-loop iteration boundaries).
flushStore :: EGraph -> IO ()
flushStore eg = case _classStore eg of
                  Nothing -> pure ()
                  Just h  -> cpsFlush h

-- | Seed the derived DBs (pattern trie and size/fitness/DL range DBs) purely
-- from the structure and @fit@ tables, without materializing any e-class page.
-- This is the lazy path's analogue of 'rebuildDBs' (which reads @_eClass@) for
-- a graph whose resident e-class map starts empty ('loadGraphLazy').
--
-- Pattern-match entries are structural only: unlike the eager path we do not
-- substitute known-constant classes (that would require reading the pages).
-- Range/size/unevaluated sets come from the @fit@ table, which is sourced from
-- the DB just like in 'rebuildDBs'.
seedEDB
  :: Int -> Bool
  -> HashMap.HashMap ENode EClassId
  -> IntMap.IntMap (Maybe Double, Maybe Double, Int, [Target])
  -> EGraphDB
seedEDB nextId trackDBs nodeToEClass fitMap =
  IntMap.foldlWithKey' step pat fitMap
  where
    trie0 :: EGraphDB
    trie0 = (emptyDB){ _nextId = nextId, _trackDBs = trackDBs }

    -- pattern trie: one path per (root class, children) per operator
    pat = HashMap.foldlWithKey' addNode trie0 nodeToEClass
    addNode db en eid =
      let ids = eid : eChildren en
          op  = eOpKey en
          cur = Map.lookup op (_patDB db)
      in case populate cur ids of
           Nothing -> db
           Just t  -> db { _patDB = Map.insert op t (_patDB db) }

    -- size/fitness/DL range DBs + unevaluated set
    step db eid (fitM, dlM, sz, _theta) =
      let db1 = db { _sizeDB = IntMap.insertWith IntSet.union sz (IntSet.singleton eid) (_sizeDB db) }
          db2 = case fitM of
                  Nothing -> db1 { _unevaluated = IntSet.insert eid (_unevaluated db1) }
                  Just fn -> db1 { _fitRangeDB = insertRange eid fn (_fitRangeDB db1)
                                 , _sizeFitDB = IntMap.insertWith RangeSet.union sz (RangeSet.singleton (fn, eid)) (_sizeFitDB db1) }
          db3 = case dlM of
                  Nothing -> db2
                  Just dn -> db2 { _dlRangeDB = insertRange eid dn (_dlRangeDB db2)
                                 , _sizeDLDB = IntMap.insertWith RangeSet.union sz (RangeSet.singleton (dn, eid)) (_sizeDLDB db2) }
      in db3

-- | Minimal DB seed for the lazily paged path: the pattern trie plus base
-- scalars (next id, tracking), but NO size/fitness/DL range DBs. The out-of-core
-- eqsat only reads '_patDB' (the matcher) and the work/analysis/unevaluated
-- sets; the range DBs are in-memory query structures that would otherwise waste
-- O(#classes) memory on every loadGraphLazy.
seedEDBPaged
  :: Int -> Bool
  -> HashMap.HashMap ENode EClassId
  -> EGraphDB
seedEDBPaged nextId trackDBs nodeToEClass =
  HashMap.foldlWithKey' addNode trie0 nodeToEClass
  where
    trie0 :: EGraphDB
    trie0 = (emptyDB){ _nextId = nextId, _trackDBs = trackDBs }
    addNode db en eid =
      let ids = eid : eChildren en
          op  = eOpKey en
          cur = Map.lookup op (_patDB db)
      in case populate cur ids of
           Nothing -> db
           Just t  -> db { _patDB = Map.insert op t (_patDB db) }

-- | Reconstruct an e-graph for out-of-core use: like 'loadGraph' but the
-- resident e-class map is left empty and an 'EClassPageStore' handle is
-- installed so classes are streamed in and out of a bounded cache. Structure
-- (canonical map, node -> class) and the derived DBs come from the relational
-- tables; individual classes are fetched lazily from the page store.
--
-- This bounds peak memory (the whole class set is never resident at once).
-- Use it with 'MonadIO'-based ('ClassStore') operations; the pure instances
-- expect a complete resident map and are not suitable for a lazy graph.
loadGraphLazy :: SqlBackend db => db -> Int -> IO (Either String EGraph)
loadGraphLazy db dsid = do
  m <- readMeta db
  case m of
    Nothing -> pure (Left "srtree-db: no e-graph stored in this database")
    Just (nextId, trackDBs) -> do
      enodes <- readNodes db
      ecLst  <- readClasses db
      dsFit  <- readDatasetFit db dsid
      let canon0  = IntMap.fromList [ (eid, c) | (eid, c, _) <- ecLst ]
          rep eid = IntMap.findWithDefault eid eid canon0
          nodeToEClass0 = HashMap.fromList enodes
          nodeToEClass  = HashMap.map rep nodeToEClass0
          fitMap = IntMap.fromList [ (eid, (f, d, sz, parseTheta (T.unpack th)))
                                   | (eid, (f, d, sz, th)) <- dsFit ]
      ps <- openClassStore db defaultClassCap 1000
      pages <- allPages ps
      if null pages
        then do
          -- fully relational fallback (databases written before the page store)
          parents <- readParents db
          let storedParents = IntMap.fromListWith Set.union
                [ (c, Set.singleton (pEid, pEn))
                | (c, pEid, pEn) <- parents ]
              classes = buildClasses canon0 nodeToEClass storedParents fitMap
                        (IntMap.fromList [ (eid, h) | (eid, _, h) <- ecLst ])
              rows    = GraphRows canon0 nodeToEClass classes nextId trackDBs
          pure (importEGraph rows)
        else do
          -- lazily paged: empty resident map + store handle + seeded DBs.
          -- Use the minimal seed (pattern DB only, no size/fitness/DL range DBs):
          -- runEqSat only reads _patDB; the range DBs are in-memory query
          -- structures, so building them here wastes O(#classes) memory.
          -- Fitness is dataset metadata, applied on page reads (not baked into
          -- the structural page blobs).
          let base = classStoreHandle ps
              h    = base { cpsLookup = \eid ->
                              fmap (fmap (applyDsFit fitMap eid)) (cpsLookup base eid) }
              eDB  = seedEDBPaged nextId trackDBs nodeToEClass
              eg   = EGraph canon0 nodeToEClass IntMap.empty eDB (Just h)
          pure (Right eg)

-- | Apply a dataset's fitness metadata to a class read from the structural page
-- store (fitness/dl/theta/size are dataset-specific, so they are attached on
-- read rather than stored in the page blob).
applyDsFit
  :: IntMap.IntMap (Maybe Double, Maybe Double, Int, [Target])
  -> EClassId -> EClass -> EClass
applyDsFit m eid ec = case IntMap.lookup eid m of
  Nothing -> ec
  Just (f, dl, sz, th) -> ec { _info = (_info ec) { _fitness = f, _dl = dl, _size = sz, _theta = th } }

readMeta :: SqlBackend db => db -> IO (Maybe (Int, Bool))
readMeta db = do
  rows <- query db "SELECT key, value FROM meta" []
  let m = HashMap.fromList [ (sqlToText k, sqlToText v) | [k, v] <- rows ]
  case HashMap.lookup "next_id" m of
    Nothing -> pure Nothing
    Just v  -> pure (Just (fromMaybe 0 (listToMaybe [ i | (i, "") <- reads (T.unpack v) ])
                          , HashMap.lookupDefault "0" "track_dbs" m == "1"))

-- | Read (e-node, its e-class) pairs from the enode + eclass_node tables.
readNodes :: SqlBackend db => db -> IO [(ENode, EClassId)]
readNodes db = do
  rows <- query db
    "SELECT n.enode_key, n.eid FROM eclass_node n JOIN enode e ON e.key = n.enode_key" []
  pure (catMaybes
    [ do
        en <- parseEnodeKey (T.unpack (sqlToText k))
        pure (en, sqlToInt eid)
    | [k, eid] <- rows ])

-- | Read (eid, canonical, height) triples.
readClasses :: SqlBackend db => db -> IO [(EClassId, EClassId, Int)]
readClasses db = do
  rows <- query db "SELECT eid, canonical, height FROM eclass" []
  pure [ (sqlToInt eid, sqlToInt c, sqlToInt h) | [eid, c, h] <- rows ]

-- | Read (child e-class, parent e-class, parent e-node) edges from the @parent@
-- table. The rows are grouped per child class by 'loadGraph'.
readParents :: SqlBackend db => db -> IO [(EClassId, EClassId, ENode)]
readParents db = do
  rows <- query db "SELECT child_eid, parent_eid, parent_enode_key FROM parent" []
  pure (catMaybes
    [ do
        en <- parseEnodeKey (T.unpack (sqlToText k))
        pure (sqlToInt c, sqlToInt p, en)
    | [c, p, k] <- rows ])

-- | Read (eid, fitness, dl, size, theta) rows.
readFit :: SqlBackend db => db -> IO [(EClassId, (Maybe Double, Maybe Double, Int, [Target]))]
readFit db = do
  rows <- query db "SELECT eid, fitness, dl, size, theta FROM fit" []
  pure [ ( sqlToInt eid
         , ( sqlToMaybeDouble f
           , sqlToMaybeDouble d
           , sqlToInt s
           , parseTheta (T.unpack (sqlToText t)) ) )
       | [eid, f, d, s, t] <- rows ]

-- | Rebuild @_grEClasses@ rows: only canonical roots carry real class rows.
-- Parent pointers come from the stored @parent@ relation when present, falling
-- back to recomputation from the node -> class map (e.g. databases written
-- before the @parent@ table existed, or hand-built rows).
buildClasses
  :: IntMap.IntMap EClassId                       -- ^ canonical eid -> eid (self-map for roots)
  -> HashMap.HashMap ENode EClassId               -- ^ node -> class
  -> IntMap.IntMap (Set.HashSet (EClassId, ENode)) -- ^ stored parent edges per class
  -> IntMap.IntMap (Maybe Double, Maybe Double, Int, [Target])  -- ^ fit data
  -> IntMap.IntMap Int                            -- ^ eid -> height
  -> IntMap.IntMap EClassRow
buildClasses canon nodeToEClass storedParents fit heights =
  IntMap.fromList [ (eid, mkRow eid) | (eid, c) <- IntMap.toList canon, c == eid ]
  where
    parentsOf :: IntMap.IntMap (Set.HashSet (EClassId, ENode))
    parentsOf = IntMap.fromListWith Set.union
      [ (c, Set.singleton (eid, en))
      | (en, eid) <- HashMap.toList nodeToEClass
      , c <- enodeChildren en ]

    mkRow :: EClassId -> EClassRow
    mkRow eid =
      let nodes = Set.fromList [ en | (en, eid') <- HashMap.toList nodeToEClass, eid' == eid ]
          h     = IntMap.findWithDefault 0 eid heights
          (fitM, dlM, sz, theta) = IntMap.findWithDefault (Nothing, Nothing, 0, []) eid fit
          stored = IntMap.findWithDefault Set.empty eid storedParents
          parents = if Set.null stored
                      then IntMap.findWithDefault Set.empty eid parentsOf
                      else stored
      in EClassRow
           { _rcNodes   = nodes
           , _rcParents = parents
           , _rcHeight  = h
           , _rcInfo    = EData 0 (headOrDefault (EVar 0) (Set.toList nodes)) NotConst
                               fitM dlM theta sz }

    headOrDefault :: a -> [a] -> a
    headOrDefault def []    = def
    headOrDefault _   (x:_) = x

-- | Push the graph's risk metrics into the @dataset_fit@ table for a dataset
-- (leaves structure intact).
pushFit :: SqlBackend db => db -> Int -> EGraph -> IO ()
pushFit db dsid eg = do
  createSchema db
  let rows0 = exportEGraph eg
  gr <- graphClassRows eg
  run db "DELETE FROM dataset_fit WHERE dataset_id = ?" [SqlInteger (fromIntegral dsid)]
  writeDatasetFitRows db dsid (rows0 { _grEClasses = gr })

-- | Overwrite in-memory fitness/DL with the values currently stored in the
-- database for a dataset (per e-class, by canonical id).
refreshFitness :: SqlBackend db => db -> Int -> EGraph -> IO (Either String EGraph)
refreshFitness db dsid eg = do
  dsFit <- readDatasetFit db dsid
  let m = forM_ dsFit $ \(eid, (fitM, _, _, theta)) ->
            case fitM of
              Nothing -> pure ()
              Just f  -> do
                c <- canonical eid
                insertFitness c f (parseTheta (T.unpack theta))
  pure (Right (runIdentity $ execStateT m eg))