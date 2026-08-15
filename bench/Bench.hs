{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

-- | Out-of-core feasibility spike.
--
-- Measures the cost of paging individual e-classes in and out of a SQL
-- database under the two access shapes the e-graph algorithm produces, and
-- compares against a RAM-resident map.
--
--   * *worklist*  - 90% of accesses hit a 10% "hot" set (congruence/rebuild
--                   locality), 10% are cold.
--   * *random*    - uniform random classes (worst case, cache-hostile).
--
-- Each class is stored as one row (a binary page). A tiny FIFO cache models
-- the future ClassStore; dirty pages are coalesced and flushed in batches.
--
-- Usage: Bench [nClasses] [ops]   (defaults 100000 200000)
-- Env:   PGDSN=<postgresql://...>  (optional; adds PostgreSQL cells)

module Main (main) where

import Control.Exception (bracket, catch, SomeException)
import Control.Monad (forM, forM_, when)
import Data.IORef
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Environment (lookupEnv, getArgs)
import System.Directory (removeFile)

import Database.SQLite3 (Database, open, close)
import Database.PostgreSQL.LibPQ (Connection)

import Algorithm.EqSat.Storage.Backend (SqlValue(..), SqlBackend(..))
import Algorithm.EqSat.Storage.SQLite ()
import Algorithm.EqSat.Storage.Postgres (connectPostgres, closePostgres)

-- ---------------------------------------------------------------------------
-- pseudo-random (LCG); deterministic and dependency-free

next :: Int -> Int
next r = (r * 1103515245 + 12345) `mod` 2147483647

pageBytes :: Int -> BS.ByteString
pageBytes eid = BS.pack (take 512 (byteLoop (eid * 2654435761 + 7)))
  where
    byteLoop r = let r' = next r in fromIntegral (r' `mod` 256) : byteLoop r'

pageHex :: Int -> T.Text
pageHex eid = T.pack (concatMap go (BS.unpack (pageBytes eid)))
  where
    go b = let hi = fromIntegral (b `div` 16); lo = fromIntegral (b `mod` 16)
           in "0123456789abcdef" !! hi : ["0123456789abcdef" !! lo]

-- ---------------------------------------------------------------------------
-- LRU page cache
--
--   * @cMap@    - eid -> (content, last-access tick)
--   * @cClock@  - tick -> eid  (ordered index for O(log n) eviction)
--   * @cPend@   - dirty pages awaiting write-back
-- All operations are O(log n).

data Cache = Cache
  { cMap   :: !(IM.IntMap (BS.ByteString, Int)) -- eid -> (content, tick)
  , cClock :: !(Map.Map Int Int)                -- tick -> eid
  , cPend  :: !(IM.IntMap BS.ByteString)        -- dirty pages awaiting write-back
  , cSize  :: !Int                              -- |cMap|  (O(1), avoid IM.size)
  , cPendN :: !Int                              -- |cPend| (O(1), avoid IM.size)
  , cTick  :: !Int
  }

emptyCache :: Cache
emptyCache = Cache IM.empty Map.empty IM.empty 0 0 0

-- | Record an access to @eid@ (content known, page resident).
touch :: Int -> BS.ByteString -> Cache -> Cache
touch eid page cc =
  let t = cTick cc + 1
      wasResident = IM.member eid (cMap cc)
      old = snd <$> IM.lookup eid (cMap cc)
      rest = IM.insert eid (page, t) (cMap cc)
      clk' = maybe (cClock cc) (`Map.delete` cClock cc) old
      sz' = if wasResident then cSize cc else cSize cc + 1
  in cc { cMap = rest, cClock = Map.insert t eid clk', cTick = t, cSize = sz' }

-- | Insert (or re-touch) @eid@, evicting the LRU page when at capacity.
-- Evicted dirty pages stay in @cPend@ until flushed.
insertEvict :: Int -> Int -> BS.ByteString -> Cache -> Cache
insertEvict cap eid page cc
  | cap <= 0 = cc
  | cSize cc < cap = touch eid page cc
  | otherwise = case Map.lookupMin (cClock cc) of
      Nothing -> touch eid page cc
      Just (tOld, ev) ->
        let cc' = cc { cMap = IM.delete ev (cMap cc)
                     , cClock = Map.delete tOld (cClock cc)
                     , cSize = cSize cc - 1 }
        in touch eid page cc'

markDirty :: Int -> Cache -> Cache
markDirty eid cc =
  case IM.lookup eid (cMap cc) of
    Nothing -> cc
    Just (bs, _) ->
      let wasDirty = IM.member eid (cPend cc)
      in cc { cPend = IM.insert eid bs (cPend cc)
            , cPendN = if wasDirty then cPendN cc else cPendN cc + 1 }

-- ---------------------------------------------------------------------------
-- workload runners

type LoadFn  = Int -> IO BS.ByteString
type FlushFn = [(Int, BS.ByteString)] -> IO ()

-- | Run @nOps@ accesses under the given access shape; returns hit count.
runPaged :: Int -> Int -> Int -> Int -> Double -> Double -> Bool
         -> LoadFn -> FlushFn -> IO Int
runPaged cap nOps n flushEvery hotProb rmwProb useWorklist load flush = do
  cacheRef <- newIORef emptyCache
  hitsRef  <- newIORef (0 :: Int)
  let nHot = max 1 (n `div` 10)
      flushNow = do
        c <- readIORef cacheRef
        let pend = IM.toList (cPend c)
        when (not (null pend)) $ do
          flush pend
          modifyIORef' cacheRef (\cc -> cc { cPend = IM.empty, cPendN = 0 })
      step r = do
        let r1   = next r
            r2   = next r1
            r3   = next r2
            p1   = fromIntegral (r2 `mod` 1000) / 1000
            p2   = fromIntegral (r3 `mod` 1000) / 1000
            eid  = if useWorklist && p1 < hotProb
                     then r1 `mod` nHot
                     else r3 `mod` n
            rmw  = p2 < rmwProb
        c0 <- readIORef cacheRef
        if IM.member eid (cMap c0)
          then do
            modifyIORef' hitsRef (+1)
            when rmw (modifyIORef' cacheRef (markDirty eid))
            pure r3
          else do
            page <- load eid
            modifyIORef' cacheRef (insertEvict cap eid page)
            when rmw (modifyIORef' cacheRef (markDirty eid))
            c1 <- readIORef cacheRef
            when (cPendN c1 >= flushEvery) flushNow
            pure r3
      go !acc r
        | acc <= 0  = pure ()
        | otherwise = step r >>= go (acc - 1)
  _ <- go nOps 12345
  flushNow
  readIORef hitsRef

-- | RAM baseline: all pages resident; every op is an IntMap lookup/update.
runRAM :: Int -> Int -> Double -> IO ()
runRAM n nOps rmwProb = do
  ref <- newIORef (IM.fromList [ (i, pageBytes i) | i <- [0 .. n - 1] ])
  let step r = do
        let r1 = next r
            eid = r1 `mod` n
        modifyIORef' ref (IM.alter (fmap modPage) eid)
        pure (next r1)
      modPage b = BS.take 1 b <> BS.drop 1 b
      go !acc r
        | acc <= 0  = pure ()
        | otherwise = step r >>= go (acc - 1)
  _ <- go nOps 12345
  pure ()

-- ---------------------------------------------------------------------------
-- table setup / bulk load

sqliteDDL, pgDDL :: T.Text
sqliteDDL = "CREATE TABLE IF NOT EXISTS kpage (eid INTEGER PRIMARY KEY, blob TEXT NOT NULL)"
pgDDL     = "CREATE TABLE IF NOT EXISTS kpage (eid BIGINT PRIMARY KEY, blob TEXT NOT NULL)"

setupSQLite :: Database -> IO ()
setupSQLite db = do
  execDb db "DROP TABLE IF EXISTS kpage"
  execDb db "PRAGMA journal_mode=WAL"
  execDb db sqliteDDL

setupPG :: Connection -> IO ()
setupPG conn = do
  execDb conn "DROP TABLE IF EXISTS kpage"
  execDb conn pgDDL

bulkLoad :: SqlBackend db => db -> Int -> IO ()
bulkLoad db n = do
  execDb db "BEGIN"
  let chunk lo = forM_ [lo .. lo + 4999] $ \i ->
        runDb db "INSERT INTO kpage (eid, blob) VALUES (?, ?)"
          [ SqlInteger (fromIntegral i), SqlText (pageHex i) ]
  forM_ [0, 5000 .. n - 1] chunk
  execDb db "COMMIT"

mkLoad :: SqlBackend db => db -> Int -> IO BS.ByteString
mkLoad db eid = do
  rows <- queryDb db "SELECT blob FROM kpage WHERE eid = ?" [SqlInteger (fromIntegral eid)]
  case rows of
    [[SqlText t]] -> pure (unhex (T.unpack t))
    _             -> fail ("missing page " <> show eid)

unhex :: String -> BS.ByteString
unhex = BS.pack . go
  where
    go (a:b:r) = fromIntegral (hex a * 16 + hex b) : go r
    go _       = []
    hex c | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
          | otherwise            = fromEnum c - fromEnum 'a' + 10

-- | Write back a batch of dirty pages in one transaction.
mkFlush :: SqlBackend db => db -> [(Int, BS.ByteString)] -> IO ()
mkFlush db pend = do
  execDb db "BEGIN"
  forM_ pend $ \(eid, _bs) ->
    runDb db "UPDATE kpage SET blob = blob WHERE eid = ?" [SqlInteger (fromIntegral eid)]
  execDb db "COMMIT"

-- ---------------------------------------------------------------------------
-- driver

data Cell = Cell String Double Double  -- label, ops/sec, hit%

timeIO :: IO a -> IO (a, Double)
timeIO act = do
  t0 <- getCurrentTime
  x  <- act
  t1 <- getCurrentTime
  pure (x, realToFrac (diffUTCTime t1 t0))

runRAMCase :: String -> Int -> Int -> Double -> IO Cell
runRAMCase lbl n ops rmw = do
  (_, s) <- timeIO (runRAM n ops rmw)
  pure (Cell lbl (fromIntegral ops / s) 100)

runPgCase :: String -> Int -> Int -> Double -> Bool -> IO Cell
runPgCase lbl n ops rmw worklist = do
  dsn <- lookupEnv "PGDSN"
  case dsn of
    Nothing -> pure (Cell (lbl <> " [skipped]") 0 0)
    Just d  -> bracket (connectPostgres d) closePostgres $ \conn -> do
      setupPG conn
      bulkLoad conn n
      (hits, s) <- timeIO $ runPaged (n `div` 4) ops n 2000 0.9 rmw worklist
                                       (mkLoad conn) (mkFlush conn)
      pure (Cell lbl (fromIntegral ops / s) (100 * fromIntegral hits / fromIntegral ops))

runSQLiteCase :: String -> Int -> Int -> Double -> Int -> Bool -> IO Cell
runSQLiteCase lbl n ops rmw cap worklist = do
  let path = "/tmp/opencode/bench.sqlite"
  removeFile path `catch` (\(_ :: SomeException) -> pure ())
  bracket (open (T.pack path)) close $ \db -> do
    setupSQLite db
    bulkLoad db n
    (hits, s) <- timeIO $ runPaged cap ops n 2000 0.9 rmw worklist
                                     (mkLoad db) (mkFlush db)
    pure (Cell lbl (fromIntegral ops / s) (100 * fromIntegral hits / fromIntegral ops))

-- | Same as the SQLite case but with a trivial in-memory "load": isolates
-- pure cache overhead from database I/O.
runNullCase :: String -> Int -> Int -> Double -> Int -> Bool -> IO Cell
runNullCase lbl n ops rmw cap worklist = do
  let page0 = pageBytes 0  -- pre-forced; cache never rebuilds it
  (hits, s) <- timeIO $ runPaged cap ops n 2000 0.9 rmw worklist
                                 (\_ -> pure page0) (\_ -> pure ())
  pure (Cell lbl (fromIntegral ops / s) (100 * fromIntegral hits / fromIntegral ops))

-- | Microbenchmarks of the container primitives used by the LRU cache.
microMaps :: Int -> IO Cell
microMaps n = do
  let bigIM = IM.fromList [ (i, (BS.replicate 512 0, i)) | i <- [0 .. n - 1] ]
      bigM  = Map.fromList [ (i, i) | i <- [0 .. n - 1] ]
      loopIM !acc !i m = if acc <= 0 then pure () else loopIM (acc - 1) (i + 1) (IM.insert i (BS.replicate 512 0, i) m)
      loopM  !acc !i m = if acc <= 0 then pure () else loopM  (acc - 1) (i + 1) (Map.insert i i m)
      loopMem !acc !i m = if acc <= 0 then pure () else loopMem (acc - 1) (i + 1) (if IM.member (i `mod` n) m then m else m)
  (_, s1) <- timeIO (loopIM 2000000 0 bigIM)
  (_, s2) <- timeIO (loopM  2000000 0 bigM)
  (_, s3) <- timeIO (loopMem 5000000 0 bigIM)
  -- replicate 'touch' on a ~full cache (insert + clock insert + eviction)
  let touchLoop !acc i (m :: IM.IntMap (BS.ByteString, Int)) (clk :: Map.Map Int Int) =
        if acc <= 0 then pure () else
          let t = i
              eid = i `mod` n
              (m2, clk2) = case Map.lookupMin clk of
                Nothing -> (IM.insert eid (BS.replicate 512 0, t) m, Map.insert t eid clk)
                Just (tOld, ev) ->
                  ( IM.insert eid (BS.replicate 512 0, t) (IM.delete ev m)
                  , Map.insert t eid (Map.delete tOld clk) )
          in touchLoop (acc - 1) (i + 1) m2 clk2
  (_, s4) <- timeIO (touchLoop 2000000 0 bigIM (Map.fromList [(i, i) | i <- [0 .. 49999]]))
  -- exact replication of runPaged's miss branch on an IORef'd Cache
  let cap = 25000
      mkPage = BS.replicate 512 0
      missLoop !acc i = if acc <= 0 then pure () else do
        ref <- newIORef emptyCache
        let one ref i = do
              c0 <- readIORef ref
              let eid = i `mod` 100000
                  hit = IM.member eid (cMap c0)
              if hit
                then modifyIORef' ref (markDirty eid)
                else do
                  modifyIORef' ref (insertEvict cap eid mkPage)
                  c1 <- readIORef ref
                  when (cPendN c1 >= 2000) (pure ())
              pure (i + 1)
        one ref i >>= missLoop (acc - 1)
  (_, s5) <- timeIO (missLoop 200000 0)
  -- same miss branch but with a persistent IORef'd cache (grows to full)
  let missLoopP !acc ref = if acc <= 0 then pure () else do
        c0 <- readIORef ref
        let eid = (acc + 1000) `mod` 100000
            hit = IM.member eid (cMap c0)
        if hit
          then modifyIORef' ref (markDirty eid)
          else do
            modifyIORef' ref (insertEvict cap eid mkPage)
            c1 <- readIORef ref
            when (cPendN c1 >= 2000) (pure ())
        missLoopP (acc - 1) ref
  pRef <- newIORef emptyCache
  (_, s5p) <- timeIO (missLoopP 200000 pRef)
  -- verbatim copy of runPaged's step/go, cache enabled vs disabled
  let mkPage = BS.replicate 512 0
      stepW cap' cacheRef hitsRef r = do
        let r1 = next r; r2 = next r1; r3 = next r2
            p1 = fromIntegral (r2 `mod` 1000) / 1000
            p2 = fromIntegral (r3 `mod` 1000) / 1000
            eid = r3 `mod` 100000
            rmw = p2 < 0.3
        c0 <- readIORef cacheRef
        if IM.member eid (cMap c0)
          then do
            modifyIORef' hitsRef (+1)
            when rmw (modifyIORef' cacheRef (markDirty eid))
            pure r3
          else do
            let page = mkPage
            modifyIORef' cacheRef (insertEvict cap' eid page)
            when rmw (modifyIORef' cacheRef (markDirty eid))
            c1 <- readIORef cacheRef
            when (cPendN c1 >= 2000) (pure ())
            pure r3
      goW cacheRef hitsRef !acc r = if acc <= 0 then pure () else stepW 25000 cacheRef hitsRef r >>= goW cacheRef hitsRef (acc - 1)
      goW0 cacheRef hitsRef !acc r = if acc <= 0 then pure () else stepW 0 cacheRef hitsRef r >>= goW0 cacheRef hitsRef (acc - 1)
      runW goF = do
        cacheRef <- newIORef emptyCache
        hitsRef <- newIORef (0 :: Int)
        goF cacheRef hitsRef 200000 12345
  (_, s6) <- timeIO (runW goW)
  (_, s7) <- timeIO (runW goW0)
  pure (Cell ("micro-IM.insert(" <> show (round (2000000 / s1)) <> "ops/s) Map.insert("
              <> show (round (2000000 / s2)) <> ") IM.member("
              <> show (round (5000000 / s3)) <> ") touch("
              <> show (round (2000000 / s4)) <> ") missbranch("
              <> show (round (200000 / s5)) <> ") missbranchP("
              <> show (round (200000 / s5p)) <> ") step-cap25("
              <> show (round (200000 / s6)) <> ") step-cap0("
              <> show (round (200000 / s7)) <> ")") 0 0)

-- | Bare loop floor: 5M iterations of LCG + IORef counter, nothing else.
selfbench :: IO Cell
selfbench = do
  ref <- newIORef (0 :: Int)
  let go !acc !r
        | acc <= 0  = pure ()
        | otherwise = go (acc - 1) (next r)
      go2 !acc !r
        | acc <= 0  = pure ()
        | otherwise = do
            let r1 = next r
            modifyIORef' ref (+1)
            go2 (acc - 1) r1
  (_, s1) <- timeIO (go 5000000 12345)
  (_, s2) <- timeIO (go2 5000000 12345)
  pure (Cell ("selfbench-loop(" <> show (round (5000000 / s1)) <> ",withIORef=" <> show (round (5000000 / s2)) <> ")") 0 0)

main :: IO ()
main = do
  args <- getArgs
  let n    = case args of (a:_)  -> read a; _ -> 100000
      ops  = case args of (_:b:_) -> read b; _ -> 200000
      rmw  = 0.3
      q1   = n `div` 4
      q2   = n `div` 2
  putStrLn ("# classes=" <> show n <> " ops=" <> show ops <> " flushEvery=2000 rmw=0.3")
  putStrLn "# cell\tops/sec\thit%"
  sb <- selfbench
  putStrLn (let Cell l _ _ = sb in l)
  mm <- microMaps 100000
  putStrLn (let Cell l _ _ = mm in l)
  cRam <- runRAMCase "ram-random-resident" n ops rmw
  cSu  <- runSQLiteCase "sqlite-uncached-random" n ops rmw 0 False
  cSw10 <- runSQLiteCase "sqlite-worklist-cap25" n ops rmw q1 True
  cSw50 <- runSQLiteCase "sqlite-worklist-cap50" n ops rmw q2 True
  cSr  <- runSQLiteCase "sqlite-random-cap25"   n ops rmw q1 False
  cR0  <- runSQLiteCase "sqlite-worklist-cap25-normw" n ops 0 q1 True
  cR5  <- runSQLiteCase "sqlite-worklist-cap25-rmw50" n ops 0.5 q1 True
  cN0  <- runNullCase "null-worklist-cap25" n ops rmw q1 True
  cN1  <- runNullCase "null-random-cap25" n ops rmw q1 False
  cN2  <- runNullCase "null-random-cap0" n ops rmw 0 False
  cN3  <- runNullCase "null-random-cap100" n ops rmw n False
  cPu  <- runPgCase "pg-uncached-random" n ops rmw False
  cPw  <- runPgCase "pg-worklist-cap25" n ops rmw True
  mapM_ (\(Cell l v h) -> putStrLn (l <> "\t" <> show (round v) <> "\t" <> show (round h)))
    [cRam, cSu, cSw10, cSw50, cSr, cR0, cR5, cN0, cN1, cN2, cN3, cPu, cPw]
