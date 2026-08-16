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
--   * Pages are stored hex-encoded in a TEXT column so the same DDL/CRUD
--     runs unchanged on SQLite and PostgreSQL (a bytea/BLOB column is a
--     possible later optimization).
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
  , hex
  , unhex
  ) where

import Control.Monad (forM_, unless, when)
import Data.IORef
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Binary (encode, decode)
import qualified Data.ByteString.Lazy as BL

import Algorithm.EqSat.Storage.Backend (SqlValue(..), SqlBackend(..), sqlToInt, sqlToText)
import Algorithm.EqSat.Egraph (EClass, EClassPageStore(..), _eClassId)

-- ---------------------------------------------------------------------------
-- hex encoding of page blobs (driver-neutral TEXT storage)

hex :: BS.ByteString -> T.Text
hex = T.pack . concatMap go . BS.unpack
  where
    go b =
      let hi = fromIntegral (b `div` 16)
          lo = fromIntegral (b `mod` 16)
      in "0123456789abcdef" !! hi : ["0123456789abcdef" !! lo]

unhex :: T.Text -> BS.ByteString
unhex = BS.pack . go . T.unpack
  where
    go (a:b:r) = fromIntegral (hexv a * 16 + hexv b) : go r
    go _       = []
    hexv c | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
           | otherwise            = fromEnum c - fromEnum 'a' + 10

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

-- | A paging store over a table @key TEXT PRIMARY KEY, blob TEXT NOT NULL@,
-- connected to a 'SqlBackend' database.
data PageStore db = PageStore
  { psDb        :: db
  , psTable     :: !T.Text
  , psCache     :: !(IORef PageCache)
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
     " (key TEXT PRIMARY KEY, blob TEXT NOT NULL)")
  cache <- newIORef (emptyCache cap)
  pure (PageStore db tbl cache cap flushEvery')

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
          [[SqlText hv]] -> do
            let page = unhex hv
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
        [ SqlInteger (fromIntegral eid), SqlText (hex page) ]
    execDb db "COMMIT"
    modifyIORef' (psCache ps) (\cc -> cc { pcPend = IM.empty, pcPendN = 0 })
  pure (length pend)

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
  pure [ (sqlToInt k, unhex (sqlToText b)) | [k, b] <- rows ]

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
  , cpsFlush  = writeback ps >> pure ()
  , cpsAll    = fmap (map (decode . BL.fromStrict . snd)) (allPages ps)
  , cpsKeys   = allPageKeys ps
  }