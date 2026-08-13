{-# LANGUAGE OverloadedStrings #-}

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
module Algorithm.EqSat.Storage.SQLite
  ( saveGraph
  , loadGraph
  , pushFit
  , refreshFitness
  , query
  , sqlToInt
  , sqlToMaybeDouble
  ) where

import Control.Monad (forM, forM_, when)
import Control.Monad.Identity (runIdentity)
import Control.Monad.State.Strict (execStateT)
import Data.List (find)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.IntMap as IntMap
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as Set
import Database.SQLite3
  ( Database, SQLData(..), StepResult(..)
  , exec, withStatement, bind, step, columns )

import Data.SRTree.Eval (Target)
import Algorithm.EqSat.Egraph
  ( EGraph, EClassId, ENode(..), EClassData(..), Consts(..)
  , canonical )
import Algorithm.EqSat.Info (insertFitness)
import Algorithm.EqSat.Store
  ( GraphRows(..), EClassRow(..), exportEGraph, importEGraph )
import Algorithm.EqSat.Storage.Types
import Algorithm.EqSat.Storage.Schema (createSchema)

-- ---------------------------------------------------------------------------
-- low-level helpers

run :: Database -> Text -> [SQLData] -> IO ()
run db sql params = withStatement db sql $ \stmt -> do
  bind stmt params
  _ <- step stmt
  pure ()

query :: Database -> Text -> [SQLData] -> IO [[SQLData]]
query db sql params = withStatement db sql $ \stmt -> do
  bind stmt params
  go stmt []
  where
    go stmt acc = do
      r <- step stmt
      case r of
        Done -> pure (reverse acc)
        Row  -> do
          cols <- columns stmt
          go stmt (cols : acc)

clearTables :: Database -> IO ()
clearTables db = do
  exec db "DELETE FROM meta"
  exec db "DELETE FROM enode_child"
  exec db "DELETE FROM eclass_node"
  exec db "DELETE FROM enode"
  exec db "DELETE FROM eclass"
  exec db "DELETE FROM fit"

-- ---------------------------------------------------------------------------
-- SQLData conversion

sqlToInt :: SQLData -> Int
sqlToInt (SQLInteger n) = fromIntegral n
sqlToInt (SQLText t)    = fromMaybe 0 (listToMaybe [ i | (i, "") <- reads (T.unpack t) ])
sqlToInt _              = 0

sqlToMaybeDouble :: SQLData -> Maybe Double
sqlToMaybeDouble SQLNull        = Nothing
sqlToMaybeDouble (SQLFloat d)   = Just d
sqlToMaybeDouble (SQLInteger n) = Just (fromIntegral n)
sqlToMaybeDouble (SQLText t)    = case reads (T.unpack t) of
                                    [(d, "")] -> Just d
                                    _         -> Nothing
sqlToMaybeDouble _ = Nothing

sqlToText :: SQLData -> Text
sqlToText (SQLText t)    = t
sqlToText (SQLInteger n) = T.pack (show n)
sqlToText (SQLFloat d)   = T.pack (show d)
sqlToText (SQLBlob b)    = TE.decodeUtf8 b
sqlToText SQLNull        = ""

-- ---------------------------------------------------------------------------
-- writing

-- | Persist the full e-graph (structure + risk metrics). Replaces any
-- previously stored graph in this database.
saveGraph :: Database -> EGraph -> IO (Either String ())
saveGraph db eg = do
  createSchema db
  let rows = exportEGraph eg
  exec db "BEGIN"
  clearTables db
  writeMeta db rows
  writeNodes db rows
  writeClasses db rows
  writeFit db rows
  exec db "COMMIT"
  pure (Right ())

writeMeta :: Database -> GraphRows -> IO ()
writeMeta db rows = do
  run db "INSERT OR REPLACE INTO meta (key, value) VALUES (?, ?)"
    [ SQLText "next_id", SQLText (T.pack (show (_grNextId rows))) ]
  run db "INSERT OR REPLACE INTO meta (key, value) VALUES (?, ?)"
    [ SQLText "track_dbs", SQLText (if _grTrackDBs rows then "1" else "0") ]

writeNodes :: Database -> GraphRows -> IO ()
writeNodes db rows =
  forM_ (HashMap.toList (_grENodeToEClass rows)) $ \(en, eid) -> do
    let key = enodeKey en
    run db "INSERT OR REPLACE INTO enode (key, op, op_detail) VALUES (?, ?, ?)"
      [ SQLText (T.pack key)
      , SQLText (T.pack (enodeOpTag en))
      , SQLText (T.pack (enodeOpDetail en)) ]
    run db "INSERT OR REPLACE INTO eclass_node (eid, enode_key) VALUES (?, ?)"
      [ SQLInteger (fromIntegral eid), SQLText (T.pack key) ]
    forM_ (naryChildren en) $ \(c, n) ->
      run db "INSERT OR REPLACE INTO enode_child (enode_key, child_eid, cnt) VALUES (?, ?, ?)"
        [ SQLText (T.pack key)
        , SQLInteger (fromIntegral c)
        , SQLInteger (fromIntegral n) ]

-- | Children of an ENAry node as (class, multiplicity); empty otherwise.
naryChildren :: ENode -> [(EClassId, Int)]
naryChildren (ENAry _ m) = IntMap.toList m
naryChildren _           = []

writeClasses :: Database -> GraphRows -> IO ()
writeClasses db rows =
  forM_ (IntMap.toAscList (_grCanonical rows)) $ \(eid, canon) ->
    run db "INSERT OR REPLACE INTO eclass (eid, canonical, height) VALUES (?, ?, ?)"
      [ SQLInteger (fromIntegral eid)
      , SQLInteger (fromIntegral canon)
      , SQLInteger (fromIntegral (maybe 0 _rcHeight (IntMap.lookup eid (_grEClasses rows)))) ]

writeFit :: Database -> GraphRows -> IO ()
writeFit db rows =
  forM_ (IntMap.toAscList (_grEClasses rows)) $ \(eid, r) -> do
    let info = _rcInfo r
        fit  = _fitness info
        dl   = _dl info
        sz   = _size info
    run db "INSERT OR REPLACE INTO fit (eid, fitness, dl, theta, size) VALUES (?, ?, ?, ?, ?)"
      [ SQLInteger (fromIntegral eid)
      , maybe SQLNull SQLFloat fit
      , maybe SQLNull SQLFloat dl
      , SQLText (T.pack (serializeTheta (_theta info)))
      , SQLInteger (fromIntegral sz) ]

-- ---------------------------------------------------------------------------
-- reading

-- | Reconstruct the e-graph stored by a previous 'saveGraph'.
loadGraph :: Database -> IO (Either String EGraph)
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
          classes = buildClasses canon nodeToEClass (IntMap.fromList fit) (IntMap.fromList [ (eid, h) | (eid, _, h) <- ecLst ])
          rows    = GraphRows canon nodeToEClass classes nextId trackDBs
      pure (importEGraph rows)

readMeta :: Database -> IO (Maybe (Int, Bool))
readMeta db = do
  rows <- query db "SELECT key, value FROM meta" []
  let m = HashMap.fromList [ (sqlToText k, sqlToText v) | [k, v] <- rows ]
  case HashMap.lookup "next_id" m of
    Nothing -> pure Nothing
    Just v  -> pure (Just (fromMaybe 0 (listToMaybe [ i | (i, "") <- reads (T.unpack v) ])
                          , HashMap.lookupDefault "0" "track_dbs" m == "1"))

-- | Read (e-node, its e-class) pairs from the enode + eclass_node tables.
readNodes :: Database -> IO [(ENode, EClassId)]
readNodes db = do
  rows <- query db
    "SELECT n.enode_key, n.eid FROM eclass_node n JOIN enode e ON e.key = n.enode_key" []
  pure (catMaybes
    [ do
        en <- parseEnodeKey (T.unpack (sqlToText k))
        pure (en, sqlToInt eid)
    | [k, eid] <- rows ])

-- | Read (eid, canonical, height) triples.
readClasses :: Database -> IO [(EClassId, EClassId, Int)]
readClasses db = do
  rows <- query db "SELECT eid, canonical, height FROM eclass" []
  pure [ (sqlToInt eid, sqlToInt c, sqlToInt h) | [eid, c, h] <- rows ]

-- | Read (eid, fitness, dl, size, theta) rows.
readFit :: Database -> IO [(EClassId, (Maybe Double, Maybe Double, Int, [Target]))]
readFit db = do
  rows <- query db "SELECT eid, fitness, dl, size, theta FROM fit" []
  pure [ ( sqlToInt eid
         , ( sqlToMaybeDouble f
           , sqlToMaybeDouble d
           , sqlToInt s
           , parseTheta (T.unpack (sqlToText t)) ) )
       | [eid, f, d, s, t] <- rows ]

-- | Rebuild @_grEClasses@ rows: only canonical roots carry real class rows.
-- Parent pointers are recomputed from the node -> class map.
buildClasses
  :: IntMap.IntMap EClassId                       -- ^ canonical eid -> eid (self-map for roots)
  -> HashMap.HashMap ENode EClassId               -- ^ node -> class
  -> IntMap.IntMap (Maybe Double, Maybe Double, Int, [Target])  -- ^ fit data
  -> IntMap.IntMap Int                            -- ^ eid -> height
  -> IntMap.IntMap EClassRow
buildClasses canon nodeToEClass fit heights =
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
      in EClassRow
           { _rcNodes   = nodes
           , _rcParents = IntMap.findWithDefault Set.empty eid parentsOf
           , _rcHeight  = h
           , _rcInfo    = EData 0 (headOrDefault (EVar 0) (Set.toList nodes)) NotConst
                              fitM dlM theta sz }

    headOrDefault :: a -> [a] -> a
    headOrDefault def []    = def
    headOrDefault _   (x:_) = x

-- | Push the graph's risk metrics into the @fit@ table (leaves structure intact).
pushFit :: Database -> EGraph -> IO ()
pushFit db eg = do
  createSchema db
  exec db "DELETE FROM fit"
  writeFit db (exportEGraph eg)

-- | Overwrite in-memory fitness/DL with the values currently stored in the
-- database (per e-class, by canonical id).
refreshFitness :: Database -> EGraph -> IO (Either String EGraph)
refreshFitness db eg = do
  fits <- readFit db
  let m = forM_ fits $ \(eid, (fitM, _, _, theta)) ->
            case fitM of
              Nothing -> pure ()
              Just f  -> do
                c <- canonical eid
                insertFitness c f theta
  pure (Right (runIdentity $ execStateT m eg))