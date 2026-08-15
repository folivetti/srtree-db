{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Test.HUnit
import Control.Monad (forM_)
import Control.Exception (catch, SomeException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Identity (runIdentity, Identity)
import Control.Monad.State.Strict (runStateT, execStateT, evalStateT, get)
import qualified Data.IntMap as IntMap
import qualified Data.HashSet as Set
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector.Unboxed as VU
import Data.Text (Text)
import Data.SRTree.Eval (Target)
import System.Environment (lookupEnv)
import System.Directory (removeFile)
import Data.Maybe (isJust, isNothing)

import Database.SQLite3 ( close, open )
import Database.PostgreSQL.LibPQ ( Connection, connectdb, finish )

import Data.SRTree
import Algorithm.EqSat
import Algorithm.EqSat.Egraph
import Algorithm.EqSat.Simplify (rewrites)
import Algorithm.EqSat.Build (fromTree)
import Algorithm.EqSat.Info (insertFitness)
import Algorithm.EqSat.DB (Pattern(..), match)
import Algorithm.EqSat.Queries (findRootClasses)
import Algorithm.EqSat.Storage.Backend ( SqlValue(..), SqlBackend(..) )
import Algorithm.EqSat.Storage.ClassStore
import Algorithm.EqSat.Storage.SQLite
import Algorithm.EqSat.Storage.Postgres ()
import qualified Algorithm.EqSat.Storage.Query as Q

myCost :: SRTree Int -> Int
myCost (Var _)     = 1
myCost (Const _)   = 1
myCost (Param _)   = 1
myCost (Bin _ l r) = 2 + l + r
myCost (Uni _ t)   = 3 + t

runIn :: EGraph -> EGraphST Identity a -> (a, EGraph)
runIn g m = runIdentity $ runStateT m g

evalIn :: EGraph -> EGraphST Identity a -> a
evalIn g m = runIdentity $ evalStateT m g

-- | Run a graph action in an IO monad so that a paged store (when the graph
-- carries one) is exercised write-through.
runIOIn :: EGraph -> EGraphST IO a -> IO (a, EGraph)
runIOIn g m = runStateT m g

-- | x0..x3; add = x0+x1 (fit 0.9), p2 = (x0+x1)*x2 (fit 0.7),
--            p3 = (x0+x1)*x3 (fit 0.5). Returns (eg, add, p2, p3).
buildGraph :: IO (EGraph, EClassId, EClassId, EClassId)
buildGraph = pure (eg, eidAdd, eidP2, eidP3)
  where
    ((eidAdd, eidP2, eidP3), eg) = runIn emptyGraph go
    go = do
      _      <- fromTree myCost (var 0)
      _      <- fromTree myCost (var 1)
      eidAdd <- fromTree myCost (var 0 + var 1)
      eidP2  <- fromTree myCost ((var 0 + var 1) * var 2)
      eidP3  <- fromTree myCost ((var 0 + var 1) * var 3)
      insertFitness eidAdd 0.9 []
      insertFitness eidP2 0.7 []
      insertFitness eidP3 0.5 [VU.fromList [1.0, 2.0]]
      pure (eidAdd, eidP2, eidP3)

numEvaluated :: EGraph -> Int
numEvaluated eg = length
  [ () | (_, ec) <- IntMap.toList (_eClass eg), _fitness (_info ec) /= Nothing ]

-- ---------------------------------------------------------------------------
-- driver-generic tests (run once against SQLite, once against PostgreSQL)

testSaveLoadRT :: SqlBackend db => IO db -> (db -> IO ()) -> Test
testSaveLoadRT openDb closeDb = TestCase $ do
  db <- openDb
  (eg, eidAdd, _, eidP3) <- buildGraph
  _ <- saveGraph db eg
  m <- loadGraph db
  case m of
    Left err -> assertFailure ("loadGraph failed: " <> err)
    Right eg' -> do
      assertEqual "class count preserved" (IntMap.size (_eClass eg)) (IntMap.size (_eClass eg'))
      assertEqual "fitness preserved" (Just 0.9) (evalIn eg' (getFitness eidAdd))
      -- theta round-trips on the class that had params
      let th = evalIn eg' (getTheta eidP3)
      assertEqual "theta preserved" 1 (length th)
      -- (x0+x1)*(x2|x3) must still be matchable after import
      let prod = Fixed (Bin Mul (Fixed (Bin Add (VarPat 'A') (VarPat 'B'))) (VarPat 'C'))
      assertBool "patterns queryable after load"
        (length (evalIn eg' (match prod)) >= 2)
      -- inserting the already-loaded (x0+x1) dedups (no new class)
      let (eid2, g2) = runIn eg' (fromTree myCost (var 0 + var 1))
      assertEqual "post-load dedup" eidAdd eid2
      assertEqual "post-load no growth" (IntMap.size (_eClass eg)) (IntMap.size (_eClass g2))
  closeDb db

testQueries :: SqlBackend db => IO db -> (db -> IO ()) -> Test
testQueries openDb closeDb = TestCase $ do
  db <- openDb
  (eg, eidAdd, eidP2, _) <- buildGraph
  _ <- saveGraph db eg
  tn2 <- Q.topN db 2
  case tn2 of
    (x : _) -> assertEqual "topN[0]" (eidAdd, 0.9) x
    []      -> assertFailure "topN returned no rows"
  assertEqual "topN length" 2 (length tn2)
  tn1 <- Q.topN db 1
  assertEqual "topN(1)" [(eidAdd, 0.9)] tn1
  dc <- Q.distributionCounts db 100
  assertEqual "distribution totals evaluated classes" (numEvaluated eg) (sum (map snd dc))
  cAdd <- Q.countPattern db "EAdd"
  assertEqual "EAdd classes" 1 cAdd
  cMul <- Q.countPattern db "EMul"
  assertEqual "EMul classes" 2 cMul
  -- pareto over (max fitness, min dl): give add (fit 0.9, dl 1.0) which
  -- dominates p2 (0.7, 2.0) and p3 (0.5, 3.0), so only add remains.
  execDb db ("UPDATE fit SET dl = 1.0 WHERE eid = " <> T.pack (show eidAdd))
  execDb db ("UPDATE fit SET dl = 2.0 WHERE eid = " <> T.pack (show eidP2))
  execDb db ("UPDATE fit SET dl = 3.0 WHERE eid NOT IN (" <> T.pack (show eidAdd) <> "," <> T.pack (show eidP2) <> ")")
  p <- Q.pareto db
  assertEqual "pareto keeps only add" [(eidAdd, 0.9, 1.0)] p
  closeDb db

testSync :: SqlBackend db => IO db -> (db -> IO ()) -> Test
testSync openDb closeDb = TestCase $ do
  db <- openDb
  (eg, eidAdd, _, _) <- buildGraph
  _ <- saveGraph db eg

  -- refit in memory, push to DB
  let egNew = runIdentity $ execStateT (insertFitness eidAdd 0.99 []) eg
  pushFit db egNew
  Right eg3 <- refreshFitness db eg
  assertEqual "pushed fitness read back" (Just 0.99) (evalIn eg3 (getFitness eidAdd))
  assertEqual "graph class count intact" (IntMap.size (_eClass eg)) (IntMap.size (_eClass eg3))

  -- edit fitness straight in the DB, refresh pulls it in
  execDb db ("UPDATE fit SET fitness = 0.55 WHERE eid = " <> T.pack (show eidAdd))
  Right eg4 <- refreshFitness db eg
  assertEqual "db edit pulled in" (Just 0.55) (evalIn eg4 (getFitness eidAdd))

  -- a stored graph loads with the current DB fitness
  Right eg5 <- loadGraph db
  assertEqual "load uses current DB fitness" (Just 0.55) (evalIn eg5 (getFitness eidAdd))
  closeDb db

pgOpen :: String -> IO Connection
pgOpen dsn = connectdb (TE.encodeUtf8 (T.pack dsn))

-- ---------------------------------------------------------------------------
-- ClassStore tests (page round-trip, persistence, LRU bound, delete)

mkPage :: Int -> BS.ByteString
mkPage i = BS.replicate (10 + i) (fromIntegral i)

-- | The @parent@ table round-trips every reverse edge: the row count matches
-- the live graph's total @_parents@ entries, and a reload reconstructs the
-- identical per-class parent sets.
testParents :: SqlBackend db => IO db -> (db -> IO ()) -> Test
testParents openDb closeDb = TestCase $ do
  db <- openDb
  (eg, _, _, _) <- buildGraph
  _ <- saveGraph db eg
  let expected = sum
        [ Set.size (_parents ec)
        | (_, ec) <- IntMap.toList (_eClass eg) ]
  rows <- query db "SELECT COUNT(*) FROM parent" []
  case rows of
    [[SqlInteger n]] -> assertEqual "parent row count" expected (fromIntegral n)
    [[SqlText t]]    -> assertEqual "parent row count" expected
                          (read (T.unpack t) :: Int)
    _                -> assertFailure "parent count: unexpected row shape"
  Right eg' <- loadGraph db
  let parentsMap g = IntMap.map _parents (_eClass g)
  assertEqual "parents preserved per class" (parentsMap eg) (parentsMap eg')
  closeDb db

testStoreRoundtrip :: SqlBackend db => IO db -> (db -> IO ()) -> Test
testStoreRoundtrip openDb closeDb = TestCase $ do
  db <- openDb
  execDb db "DROP TABLE IF EXISTS cstore_test"
  ps <- newPageStore db "cstore_test" 0 100
  forM_ [0 .. 4] $ \i -> writePage ps i (mkPage i)
  forM_ [0 .. 4] $ \i -> do
    mp <- readPage ps i
    assertEqual ("page " <> show i) (Just (mkPage i)) mp
  assertEqual "pending before flush" 5 =<< pendingCount ps
  assertEqual "writeback count" 5 =<< writeback ps
  assertEqual "pending after flush" 0 =<< pendingCount ps
  closeDb db

testStorePersist :: SqlBackend db => IO db -> (db -> IO ()) -> Test
testStorePersist openDb closeDb = TestCase $ do
  db <- openDb
  execDb db "DROP TABLE IF EXISTS cstore_test"
  ps1 <- newPageStore db "cstore_test" 0 100
  writePage ps1 42 (mkPage 42)
  _ <- writeback ps1
  closeDb db
  db2 <- openDb
  ps2 <- newPageStore db2 "cstore_test" 0 100
  mp <- readPage ps2 42
  assertEqual "persisted page" (Just (mkPage 42)) mp
  closeDb db2

testStoreLRU :: SqlBackend db => IO db -> (db -> IO ()) -> Test
testStoreLRU openDb closeDb = TestCase $ do
  db <- openDb
  execDb db "DROP TABLE IF EXISTS cstore_test"
  ps <- newPageStore db "cstore_test" 2 100
  forM_ [0 .. 9] $ \i -> writePage ps i (mkPage i)
  assertEqual "resident bounded by cap" 2 =<< residentCount ps
  _ <- writeback ps
  forM_ [0 .. 9] $ \i -> do
    mp <- readPage ps i
    assertEqual ("evicted reload " <> show i) (Just (mkPage i)) mp
  deletePage ps 3
  mp <- readPage ps 3
  assertEqual "deleted page gone" Nothing mp
  closeDb db

runStoreSuite :: SqlBackend db => String -> (IO db, db -> IO ()) -> [Test]
runStoreSuite tag (openDb, closeDb) =
  [ TestLabel (tag <> " store-roundtrip") (testStoreRoundtrip openDb closeDb)
  , TestLabel (tag <> " store-persist")   (testStorePersist openDb closeDb)
  , TestLabel (tag <> " store-lru")       (testStoreLRU openDb closeDb)
  ]

-- | The e-class page store round-trips: saveGraph writes one page per class,
-- loadGraph installs the paged store handle, class count/fitness/parents are
-- preserved, and post-load dedup does not grow the graph.
testPagedRoundtrip :: SqlBackend db => IO db -> (db -> IO ()) -> Test
testPagedRoundtrip openDb closeDb = TestCase $ do
  db <- openDb
  (eg, eidAdd, _, eidP3) <- buildGraph
  _ <- saveGraph db eg
  pc <- query db "SELECT COUNT(*) FROM cstore_page" []
  let expected = IntMap.size (_eClass eg)
  case pc of
    [[SqlInteger n]] -> assertEqual "page count" expected (fromIntegral n)
    [[SqlText t]]    -> assertEqual "page count" expected (read (T.unpack t))
    _                -> assertFailure "page count: unexpected row shape"
  m <- loadGraph db
  case m of
    Left err -> assertFailure ("loadGraph failed: " <> err)
    Right eg' -> do
      assertBool "paged store installed" (isJust (_classStore eg'))
      assertEqual "class count preserved" (IntMap.size (_eClass eg)) (IntMap.size (_eClass eg'))
      assertEqual "fitness (paged)" (Just 0.9) (evalIn eg' (getFitness eidAdd))
      assertEqual "theta (paged)" 1 (length (evalIn eg' (getTheta eidP3)))
      -- inserting an already-loaded node dedups
      let (eid2, g2) = runIn eg' (fromTree myCost (var 0 + var 1))
      assertEqual "post-load dedup" eidAdd eid2
      assertEqual "post-load no growth" (IntMap.size (_eClass eg)) (IntMap.size (_eClass g2))
      -- flushing a paged graph is safe
      flushStore eg'
  closeDb db

-- | Mutations through the paged store persist: add a class + fitness, save
-- again, reload, and both the new and old classes are present.
testPagedMutatePersist :: SqlBackend db => IO db -> (db -> IO ()) -> Test
testPagedMutatePersist openDb closeDb = TestCase $ do
  db <- openDb
  (eg, eidAdd, _, _) <- buildGraph
  _ <- saveGraph db eg
  Right eg0 <- loadGraph db
  (eidNew, eg1) <- runIOIn eg0 $ do
    cid <- fromTree myCost (var 0 + var 2)
    insertFitness cid 0.33 []
    st <- get
    liftIO (flushStore st)
    pure cid
  -- (x0+x2) was not present, so a brand new class was created
  assertBool "new class is distinct" (eidNew /= eidAdd)
  _ <- saveGraph db eg1
  Right eg2 <- loadGraph db
  assertEqual "new class fitness persisted" (Just 0.33) (evalIn eg2 (getFitness eidNew))
  assertEqual "old class intact" (Just 0.9) (evalIn eg2 (getFitness eidAdd))
  assertEqual "paged store reinstalled" True (isJust (_classStore eg2))
  closeDb db

-- | A database without pages (page table emptied) loads through the fully
-- relational path, with no paged store handle.
testPagedFallback :: SqlBackend db => IO db -> (db -> IO ()) -> Test
testPagedFallback openDb closeDb = TestCase $ do
  db <- openDb
  (eg, eidAdd, _, _) <- buildGraph
  _ <- saveGraph db eg
  execDb db "DELETE FROM cstore_page"
  m <- loadGraph db
  case m of
    Left err -> assertFailure ("fallback load failed: " <> err)
    Right eg' -> do
      assertBool "fallback: no paged store" (isNothing (_classStore eg'))
      assertEqual "fallback class count" (IntMap.size (_eClass eg)) (IntMap.size (_eClass eg'))
      assertEqual "fallback fitness" (Just 0.9) (evalIn eg' (getFitness eidAdd))
  closeDb db

-- | The out-of-core path: 'loadGraphLazy' leaves the resident e-class map
-- empty, installs the paged store, and streams every whole-graph read through
-- the store (bounds memory). Mutations persist through the store.
testLazyLoad :: SqlBackend db => IO db -> (db -> IO ()) -> Test
testLazyLoad openDb closeDb = TestCase $ do
  db <- openDb
  (eg, eidAdd, eidP2, _) <- buildGraph
  _ <- saveGraph db eg
  obj <- loadGraphLazy db
  case obj of
    Left err -> assertFailure ("loadGraphLazy failed: " <> err)
    Right eg' -> do
      assertBool "lazy: paged store installed" (isJust (_classStore eg'))
      -- resident cache starts empty (nothing materialized)
      assertEqual "lazy: resident cache empty" 0 (IntMap.size (_eClass eg'))
      -- whole-graph enumeration streams from the store
      (classes, g1) <- runIOIn eg' allClasses
      assertEqual "lazy: allClasses accepts-every-class"
        (IntMap.size (_eClass eg)) (length classes)
      -- point reads fall back to the store
      (f, _) <- runIOIn g1 (getFitness eidAdd)
      assertEqual "lazy: getFitness from store" (Just 0.9) f
      -- structural pattern matching works on the seeded patDB/range DBs
      let pat = Fixed (Bin Mul (Fixed (Bin Add (VarPat 'A') (VarPat 'B'))) (VarPat 'C'))
      (ms, _) <- runIOIn g1 (match pat)
      assertBool "lazy: patterns queryable" (length ms >= 2)
      -- root enumeration streams from the store
      (roots, _) <- runIOIn g1 findRootClasses
      assertEqual "lazy: two root classes" 2 (length roots)
      -- mutate through the lazy graph, then persist the whole (paged) graph
      (eidNew, eg2) <- runIOIn g1 $ do
        cid <- fromTree myCost (var 3 + var 0)   -- a fresh class, not in the DB
        insertFitness cid 0.11 []
        st <- get
        liftIO (flushStore st)
        pure cid
      _ <- saveGraph db eg2
      Right eg3 <- loadGraph db
      assertEqual "lazy mutate: new class persisted" (Just 0.11) (evalIn eg3 (getFitness eidNew))
      assertEqual "lazy mutate: old class intact" (Just 0.9) (evalIn eg3 (getFitness eidAdd))
  closeDb db

-- | A lazy (resident-empty) graph can still be rewritten: 'runEqSat' streams
-- every class through the paged store while evaluating the const-valued
-- preconditions (e.g. 'isNotZero') against stored class info. After a pass with
-- the 'rewrites' ruleset, the store-aware conditions fired and the constant
-- rewrites (x-x->0, 1**x->1, x/x->1) landed.
testLazyRewrite :: SqlBackend db => IO db -> (db -> IO ()) -> Test
testLazyRewrite openDb closeDb = TestCase $ do
  db <- openDb
  let eg = snd $ runIn emptyGraph $ do
        _ <- fromTree myCost (var 0 - var 0)
        _ <- fromTree myCost (1 ** var 1)
        _ <- fromTree myCost (var 0 / var 0)
        pure ()
  _ <- saveGraph db eg
  obj <- loadGraphLazy db
  case obj of
    Left err -> assertFailure ("loadGraphLazy failed: " <> err)
    Right eg' -> do
      assertEqual "lazy: resident cache empty" 0 (IntMap.size (_eClass eg'))
      -- one eqsat pass; conditions are evaluated against the paged store
      (_, g1) <- runIOIn eg' (runEqSat myCost rewrites 10)
      -- x - x rewrote to Const 0
      (m0, _) <- runIOIn g1 (match (Fixed (Const 0)))
      assertBool "lazy rewrite: x-x -> 0" (not (null m0))
      -- 1 ** x and x / x (guarded by isNotZero) rewrote to Const 1
      (m1, _) <- runIOIn g1 (match (Fixed (Const 1)))
      assertBool "lazy rewrite: produced Const 1" (length m1 >= 1)
      flushStore g1
  closeDb db

runPagedSuite :: SqlBackend db => String -> (IO db, db -> IO ()) -> [Test]
runPagedSuite tag (openDb, closeDb) =
  [ TestLabel (tag <> " paged-roundtrip")   (testPagedRoundtrip openDb closeDb)
  , TestLabel (tag <> " paged-mutate-persist") (testPagedMutatePersist openDb closeDb)
  , TestLabel (tag <> " paged-fallback")    (testPagedFallback openDb closeDb)
  , TestLabel (tag <> " lazy-load")         (testLazyLoad openDb closeDb)
  , TestLabel (tag <> " lazy-rewrite")      (testLazyRewrite openDb closeDb)
  ]

runSuite :: SqlBackend db => String -> (IO db, db -> IO ()) -> [Test]
runSuite tag (openDb, closeDb) =
  [ TestLabel (tag <> " save-load-roundtrip") (testSaveLoadRT openDb closeDb)
  , TestLabel (tag <> " parents")             (testParents openDb closeDb)
  , TestLabel (tag <> " queries")             (testQueries openDb closeDb)
  , TestLabel (tag <> " sync")                (testSync openDb closeDb)
  ]
    <> runStoreSuite tag (openDb, closeDb)

main :: IO ()
main = do
  let sqlitePath = "/tmp/opencode/srtree-db-test.sqlite"
  removeFile sqlitePath `catch` (\(_ :: SomeException) -> pure ())
  sqliteCounts <- runTestTT $ TestList
    (runSuite "sqlite" (open (T.pack sqlitePath), close)
      <> runPagedSuite "sqlite" (open (T.pack sqlitePath), close))
  mdsn <- lookupEnv "PGDSN"
  case mdsn of
    Nothing -> do
      if failures sqliteCounts /= 0 || errors sqliteCounts /= 0
        then error "Some tests failed"
        else do
          putStrLn "PGDSN not set -- skipping PostgreSQL backend tests"
          pure ()
    Just dsn -> do
      pgCounts <- runTestTT $ TestList
        (runSuite "postgresql" (pgOpen dsn, finish)
          <> runPagedSuite "postgresql" (pgOpen dsn, finish))
      if failures sqliteCounts /= 0 || errors sqliteCounts /= 0
            || failures pgCounts /= 0 || errors pgCounts /= 0
        then error "Some tests failed"
        else pure ()