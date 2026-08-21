{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- | A lazily loaded, LRU-cached, write-back page store for e-class blobs.
--
-- Provides the storage substrate for an out-of-core e-graph: individual
-- e-classes (serialized to 'ByteString' pages) live in a single key-value
-- table and are paged in and out of a bounded LRU cache. Dirty pages are
-- coalesced and flushed in batches, so the write-back cost is a small number
-- of transactions instead of one per mutation.
--
-- Design (validated by the 'srtree-db-bench' spike):
--
--   * LRU cache with an O(log n) ordered recency index; cache capacity is an
--     explicit bound (a fraction of the total class count).
--   * O(1) resident/pending counters (never 'Data.IntMap.size', which is
--     O(n) per call).
--   * Every access refreshes recency (true LRU), so the hot classes a
--     rebuild/rewrite pass revisits stay resident.
--   * Pages are stored as binary BLOBs for compact storage and zero-copy
--     reads.
--
-- Driver-neutrality note: the store talks only through 'SqlBackend' and
-- spells writes as DELETE+INSERT inside one transaction (both drivers
-- support these statements; no driver-specific upsert syntax).
--
-- The table is a plain key-value page store; the actual
-- 'Algorithm.EqSat.Egraph.EClass' serialization and the parent relation are
-- layered on top of it by the caller.

module Algorithm.EqSat.Storage.ClassStore
  ( PageStore
  , newPageStore
  , readPage
  , writePage
  , deletePage
  , writeback
  , pendingCount
  , residentCount
  , flushEvery
  , allPages
  , classStoreTable
  , openClassStore
  , classStoreHandle
  , frontierTable
  , markFrontier
  , loadFrontierRows
  , clearFrontier
  , setFrontierActive
  , initFrontier
  ) where

import Control.Monad (forM_, unless, when)
import Data.IORef
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS
import qualified Data.HashSet as Set
import qualified Data.Text as T
import Data.Binary (encode, decode)
import Data.List (nub)
import qualified Data.ByteString.Lazy as BL

import Algorithm.EqSat.Storage.Backend
  ( SqlValue(..), SqlBackend(..), sqlToInt, sqlToText, sqlToBlob )
import Algorithm.EqSat.Storage.Types
  ( enodeKey, enodeOpTag, enodeOpDetail, opDetailOf )
import Algorithm.EqSat.Egraph
  ( EClass, EClassPageStore(..), ENode(..), _eClassId )

-- ---------------------------------------------------------------------------
-- LRU page cache

data PageCache = PageCache
  { pcMap    :: !(IM.IntMap (BS.ByteString, Int)) -- eid -> (page, recency tick)
  , pcClock  :: !(Map.Map Int Int)                -- tick -> eid (for eviction)
  , pcPend   :: !(IM.IntMap BS.ByteString)        -- dirty pages awaiting write-back
  , pcSize   :: !Int                              -- |pcMap|  (O(1))
  , pcPendN  :: !Int                              -- |pcPend| (O(1))
  , pcTick   :: !Int
  , pcCap    :: !Int
  }

emptyCache :: Int -> PageCache
emptyCache cap = PageCache IM.empty Map.empty IM.empty 0 0 0 cap

-- | Record an access to @eid@ with the given page content, updating recency.
touch :: Int -> BS.ByteString -> PageCache -> PageCache
touch eid page pc =
  let t = pcTick pc + 1
      resident = IM.member eid (pcMap pc)
      old = snd <$> IM.lookup eid (pcMap pc)
      m' = IM.insert eid (page, t) (pcMap pc)
      clk' = maybe (pcClock pc) (`Map.delete` pcClock pc) old
      sz' = if resident then pcSize pc else pcSize pc + 1
  in pc { pcMap = m', pcClock = Map.insert t eid clk', pcSize = sz', pcTick = t }

-- | Insert (or re-touch) @eid@, evicting the least-recently-used page when at
-- capacity. Evicted dirty pages stay in @pcPend@ until written back.
-- A capacity of 0 keeps every page resident (unbounded).
insertEvict :: Int -> BS.ByteString -> PageCache -> PageCache
insertEvict eid page pc
  | pcCap pc <= 0 = touch eid page pc
  | pcSize pc < pcCap pc = touch eid page pc
  | otherwise = case Map.lookupMin (pcClock pc) of
      Nothing -> touch eid page pc
      Just (tOld, ev) ->
        touch eid page
          pc { pcMap = IM.delete ev (pcMap pc)
             , pcClock = Map.delete tOld (pcClock pc)
             , pcSize = pcSize pc - 1 }

-- | Mark @eid@ dirty (ensuring it is resident first).
markDirty :: Int -> BS.ByteString -> PageCache -> PageCache
markDirty eid page pc =
  let pc' = if IM.member eid (pcMap pc) then pc else insertEvict eid page pc
      wasDirty = IM.member eid (pcPend pc')
  in pc' { pcPend = IM.insert eid page (pcPend pc')
         , pcPendN = if wasDirty then pcPendN pc' else pcPendN pc' + 1 }

-- ---------------------------------------------------------------------------
-- store

-- | A paging store over a table @key INTEGER PRIMARY KEY, blob BLOB NOT NULL@,
-- connected to a 'SqlBackend' database.
data PageStore db = PageStore
  { psDb        :: db
  , psTable     :: !T.Text
  , psCache     :: !(IORef PageCache)
  , psNodes     :: !(IORef (Set.HashSet (Int, ENode))) -- newly-created nodes awaiting write-back
  , psCanon     :: !(IORef (IM.IntMap Int))            -- pending canonical (eid -> representative) rows
  , psFrontier  :: !(IORef IntSet.IntSet)              -- recently-changed e-classes (dirty set)
  , psFrontierActive :: !(IORef Bool)                  -- restrict the matcher to the frontier
  , psCap       :: !Int
  , psFlushEvery :: !Int
  }

-- | Open a page store: creates the (driver-neutral) page table if missing,
-- with the given bounded cache (in pages) and a write-back flush every
-- @flushEvery@ dirty pages.
--
-- The table name is a plain identifier (never interpolate user input).
newPageStore :: SqlBackend db => db -> T.Text -> Int -> Int -> IO (PageStore db)
newPageStore db tbl cap flushEvery' = do
  execDb db
    ("CREATE TABLE IF NOT EXISTS " <> tbl <>
     " (key INTEGER PRIMARY KEY, blob BLOB NOT NULL)")
  cache <- newIORef (emptyCache cap)
  nodes <- newIORef Set.empty
  canons <- newIORef IM.empty
  frontier <- newIORef IntSet.empty
  active <- newIORef False
  pure (PageStore db tbl cache nodes canons frontier active cap flushEvery')

-- | Read the page for @eid@ (cache miss loads from the database). Refreshes
-- LRU recency on hit so hot classes stay resident.
readPage :: SqlBackend db => PageStore db -> Int -> IO (Maybe BS.ByteString)
readPage ps eid = do
  c0 <- readIORef (psCache ps)
  case IM.lookup eid (pcMap c0) of
    Just (page, _) -> do
      writeIORef (psCache ps) (touch eid page c0)
      pure (Just page)
    Nothing -> case IM.lookup eid (pcPend c0) of
      -- the page is dirty (written but not yet flushed) and was evicted from
      -- the LRU cache; it is NOT in the database yet, so return it from the
      -- pending set and restore it as resident.
      Just page -> do
        writeIORef (psCache ps) (insertEvict eid page c0)
        pure (Just page)
      Nothing -> do
        rows <- queryDb (psDb ps)
                  ("SELECT blob FROM " <> psTable ps <> " WHERE key = ?")
                  [SqlInteger (fromIntegral eid)]
        case rows of
          [] -> pure Nothing
          [[SqlBlob page]] -> do
            writeIORef (psCache ps) (insertEvict eid page c0)
            pure (Just page)
          [[SqlText hv]] -> do
            -- backward compat: old databases may still have hex-encoded TEXT
            let page = sqlToBlob (SqlText hv)
            writeIORef (psCache ps) (insertEvict eid page c0)
            pure (Just page)
          _ -> fail "ClassStore.readPage: unexpected row shape"

-- | Write the page for @eid@ (resident + marked dirty). Triggers a batched
-- write-back once @flushEvery@ dirty pages have accumulated on a write.
writePage :: SqlBackend db => PageStore db -> Int -> BS.ByteString -> IO ()
writePage ps eid page = do
  modifyIORef' (psCache ps) (markDirty eid page)
  c <- readIORef (psCache ps)
  when (pcPendN c >= psFlushEvery ps) (writeback ps >> pure ())

-- | Remove @eid@ from the cache and the database.
deletePage :: SqlBackend db => PageStore db -> Int -> IO ()
deletePage ps eid = do
  modifyIORef' (psCache ps) $ \pc ->
    pc { pcMap = IM.delete eid (pcMap pc)
       , pcPend = IM.delete eid (pcPend pc)
       , pcPendN = if IM.member eid (pcPend pc) then pcPendN pc - 1 else pcPendN pc }
  runDb (psDb ps)
    ("DELETE FROM " <> psTable ps <> " WHERE key = ?")
    [SqlInteger (fromIntegral eid)]

-- | Write back all pending dirty pages in one transaction. Returns the number
-- of pages written.
writeback :: SqlBackend db => PageStore db -> IO Int
writeback ps = do
  c <- readIORef (psCache ps)
  let pend = IM.toList (pcPend c)
  unless (null pend) $ do
    let db = psDb ps
        tbl = psTable ps
    execDb db "BEGIN"
    forM_ pend $ \(eid, page) -> do
      runDb db ("DELETE FROM " <> tbl <> " WHERE key = ?")
        [SqlInteger (fromIntegral eid)]
      runDb db ("INSERT INTO " <> tbl <> " (key, blob) VALUES (?, ?)")
        [ SqlInteger (fromIntegral eid), SqlBlob page ]
    execDb db "COMMIT"
    modifyIORef' (psCache ps) (\cc -> cc { pcPend = IM.empty, pcPendN = 0 })
  flushNodes ps
  flushCanon ps
  pure (length pend)

-- | Flush any newly-created nodes recorded via 'cpsRecordNode' into the
-- relational @enode@/@eclass_node@/@enode_child@ tables (idempotently), so the
-- streaming matcher's @op_detail@ index and the @enode_child@ children table
-- reflect the live graph. Runs inside its own transaction (the page write-back
-- above may be a no-op).
flushNodes :: SqlBackend db => PageStore db -> IO ()
flushNodes ps = do
  nodes <- readIORef (psNodes ps)
  unless (Set.null nodes) $ do
    let db = psDb ps
    execDb db "BEGIN"
    forM_ (Set.toList nodes) $ \(eid, en) -> do
      let key = T.pack (enodeKey en)
      insertIgnore db "enode (key, op, op_detail) VALUES (?, ?, ?)"
        [ SqlText key
        , SqlText (T.pack (enodeOpTag en))
        , SqlText (T.pack (enodeOpDetail en)) ]
      insertIgnore db "eclass_node (eid, enode_key) VALUES (?, ?)"
        [ SqlInteger (fromIntegral eid), SqlText key ]
      -- ENAry children are stored relationally; keep them populated during eqsat
      -- too (they were previously written only at import/save).
      forM_ (naryChildren en) $ \(c, n) ->
        insertIgnore db "enode_child (enode_key, child_eid, cnt) VALUES (?, ?, ?)"
          [ SqlText key, SqlInteger (fromIntegral c), SqlInteger (fromIntegral n) ]
    execDb db "COMMIT"
    modifyIORef' (psNodes ps) (const Set.empty)

-- | Children of an ENAry node as (class, multiplicity); empty otherwise.
naryChildren :: ENode -> [(Int, Int)]
naryChildren (ENAry _ m) = IM.toList m
naryChildren _           = []

-- | Flush any pending canonical rows (recorded via 'cpsRecordCanonical') into
-- @eclass.canonical@ (idempotent upsert), so the streaming canonical lookup
-- ('cpsCanonicalOf') reflects merges and new classes. Runs in its own
-- transaction.
flushCanon :: SqlBackend db => PageStore db -> IO ()
flushCanon ps = do
  canons <- readIORef (psCanon ps)
  unless (IM.null canons) $ do
    let db = psDb ps
    execDb db "BEGIN"
    forM_ (IM.toList canons) $ \(eid, canon) ->
      runDb db ("INSERT INTO eclass (eid, canonical, height) VALUES (?, ?, 0) "
                <> "ON CONFLICT(eid) DO UPDATE SET canonical=excluded.canonical")
        [ SqlInteger (fromIntegral eid)
        , SqlInteger (fromIntegral canon) ]
    execDb db "COMMIT"
    writeIORef (psCanon ps) IM.empty

-- | Number of dirty pages not yet written back.
pendingCount :: PageStore db -> IO Int
pendingCount ps = pcPendN <$> readIORef (psCache ps)

-- | Number of resident (cached) pages.
residentCount :: PageStore db -> IO Int
residentCount ps = pcSize <$> readIORef (psCache ps)

-- | The configured flush threshold.
flushEvery :: PageStore db -> Int
flushEvery = psFlushEvery

-- ---------------------------------------------------------------------------
-- e-class page wiring

-- | Read every (eid, page blob) currently stored in the page table.
allPages :: SqlBackend db => PageStore db -> IO [(Int, BS.ByteString)]
allPages ps = do
  rows <- queryDb (psDb ps) ("SELECT key, blob FROM " <> psTable ps) []
  pure [ (sqlToInt k, sqlToBlob b) | [k, b] <- rows ]

-- | Read every e-class id currently stored in the page table (keys only). This
-- does NOT load the page blobs, so callers that only need the id set (e.g.
-- 'recalculateBestAllStream' via 'cpsKeys') don't pull the whole page store
-- into memory just to extract keys.
allPageKeys :: SqlBackend db => PageStore db -> IO [Int]
allPageKeys ps = do
  rows <- queryDb (psDb ps) ("SELECT key FROM " <> psTable ps) []
  pure [ sqlToInt k | [k] <- rows ]

-- | Table used for the lazily paged e-class store (shared by save/load).
classStoreTable :: T.Text
classStoreTable = "cstore_page"

-- | Table holding the re-saturation frontier: e-classes that have been created
-- or merged since the last frontier re-saturation pass, so a subsequent pass can
-- re-saturate only these (and classes they touch) instead of the whole graph.
frontierTable :: T.Text
frontierTable = "frontier"

-- | Mark an e-class as recently changed (part of the re-saturation frontier).
markFrontier :: PageStore db -> Int -> IO ()
markFrontier ps eid = modifyIORef' (psFrontier ps) (IntSet.insert eid)

-- | Read the persisted frontier e-class ids (for initialising a pass).
loadFrontierRows :: SqlBackend db => db -> IO [Int]
loadFrontierRows db = do
  rows <- queryDb db ("SELECT eid FROM " <> frontierTable) []
  pure [ sqlToInt e | [e] <- rows ]

-- | Whether the matcher's candidate-root enumeration is restricted to the
-- frontier (a frontier re-saturation pass). Off by default (normal eqsat and
-- the pure in-memory path are unaffected).
setFrontierActive :: PageStore db -> Bool -> IO ()
setFrontierActive ps a = writeIORef (psFrontierActive ps) a

-- | Start a frontier re-saturation: seed the in-memory dirty set from the
-- persisted frontier (or an explicit seed), and restrict the matcher to it.
initFrontier :: SqlBackend db => PageStore db -> [Int] -> IO ()
initFrontier ps seed = do
  persisted <- loadFrontierRows (psDb ps)
  writeIORef (psFrontier ps) (IntSet.fromList (persisted ++ seed))
  writeIORef (psFrontierActive ps) True

-- | End a frontier re-saturation: clear the persisted frontier and the
-- in-memory dirty set, and lift the matcher restriction.
clearFrontier :: SqlBackend db => PageStore db -> IO ()
clearFrontier ps = do
  runDb (psDb ps) ("DELETE FROM " <> frontierTable) []
  writeIORef (psFrontier ps) IntSet.empty
  writeIORef (psFrontierActive ps) False

-- | Persist the in-memory frontier (recently-changed classes) to the frontier
-- table, so it survives until the next re-saturation pass. Idempotent.
flushFrontier :: SqlBackend db => PageStore db -> IO ()
flushFrontier ps = do
  f <- readIORef (psFrontier ps)
  unless (IntSet.null f) $ do
    let db = psDb ps
    execDb db "BEGIN"
    forM_ (IntSet.toList f) $ \eid ->
      insertIgnore db ("frontier (eid, updated_at) VALUES (?, ?)")
        [ SqlInteger (fromIntegral eid), SqlText (T.pack (show (0 :: Int))) ]
    execDb db "COMMIT"
    writeIORef (psFrontier ps) IntSet.empty

-- | Open the e-class page store on 'classStoreTable', creating the table if
-- missing. @cap@ bounds the LRU cache (0 = unbounded), @flushEvery'@ is the
-- dirty-page threshold that triggers a batched write-back.
openClassStore :: SqlBackend db => db -> Int -> Int -> IO (PageStore db)
openClassStore db cap flushEvery' = newPageStore db classStoreTable cap flushEvery'

-- | Adapt a 'PageStore' into the 'EClassPageStore' handle an 'EGraph' carries:
-- e-class blobs are 'Binary'-serialized pages. The store is authoritative;
-- the graph's resident map mirrors insertions and is consulted first on reads.
classStoreHandle :: SqlBackend db => PageStore db -> EClassPageStore
classStoreHandle ps = EClassPageStore
  { cpsLookup = \eid -> fmap (fmap (decode . BL.fromStrict)) (readPage ps eid)
  , cpsInsert = \ec -> writePage ps (_eClassId ec) (BL.toStrict (encode ec))
  , cpsDelete = \eid -> deletePage ps eid
  , cpsFlush  = writeback ps >> flushFrontier ps >> pure ()
  , cpsAll    = fmap (map (decode . BL.fromStrict . snd)) (allPages ps)
  , cpsKeys   = allPageKeys ps
  , cpsStreamRoots = \op budget exclude -> do
      -- Union the DB's operator index with any not-yet-flushed nodes, so the
      -- streaming matcher sees every live node (like the resident _patDB trie),
      -- not just the last flushed snapshot. Excluded (already-attempted) roots
      -- are skipped so the per-rule budget advances to new roots. When a
      -- frontier re-saturation is active, roots are further restricted to the
      -- frontier (recently-changed) e-classes. Memory is O(budget + pending +
      -- size of exclude + size of frontier).
      dbRoots <- streamByOp (psDb ps) (T.pack (opDetailOf op)) budget exclude
      pend <- readIORef (psNodes ps)
      active <- readIORef (psFrontierActive ps)
      frontier <- readIORef (psFrontier ps)
      let detail = opDetailOf op
          ex = IntSet.fromList exclude
          pendRoots = [ eid | (eid, en) <- Set.toList pend
                            , enodeOpDetail en == detail
                            , not (IntSet.member eid ex) ]
          keep e = not active || IntSet.member e frontier
      pure (take budget (filter keep (nub (pendRoots ++ dbRoots))))
  , cpsRecordNode  = \en eid -> markFrontier ps eid >> modifyIORef' (psNodes ps) (Set.insert (eid, en))
  , cpsNodeToClass = \en -> do
      -- content-address lookup, seeing not-yet-flushed nodes first (live)
      pend <- readIORef (psNodes ps)
      case [ eid | (eid, pe) <- Set.toList pend, pe == en ] of
        (eid : _) -> pure (Just eid)
        [] -> do
          rows <- queryDb (psDb ps)
            "SELECT eid FROM eclass_node WHERE enode_key = ?"
            [SqlText (T.pack (enodeKey en))]
          pure (case rows of [[v]] -> Just (sqlToInt v); _ -> Nothing)
  , cpsCanonicalOf = \eid -> do
      pend <- readIORef (psCanon ps)
      case IM.lookup eid pend of
        Just c  -> pure (Just c)
        Nothing -> do
          rows <- queryDb (psDb ps)
            "SELECT canonical FROM eclass WHERE eid = ?"
            [SqlInteger (fromIntegral eid)]
          pure (case rows of [[v]] -> Just (sqlToInt v); _ -> Nothing)
  , cpsRecordCanonical = \eid canon -> markFrontier ps eid >> modifyIORef' (psCanon ps) (IM.insert eid canon)
  , cpsBeginFrontier = initFrontier ps []
  , cpsEndFrontier    = clearFrontier ps
  }