{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module EqSat
  ( EqSatOpts(..)
  , eqsatParser
  , runEqSatCmd
  ) where

import Control.Exception (bracket, SomeException, catch, displayException)
import Control.Monad.State.Strict (execStateT)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Text as T
import Options.Applicative
import System.CPUTime (getCPUTime)
import System.IO (hFlush, stdout)

import Data.SRTree (SRTree(..))
import Algorithm.EqSat (runEqSat)
import Algorithm.EqSat.Egraph (EGraph(..), EClassPageStore(..))
import Algorithm.EqSat.Simplify (Rule, rewrites, rewritesParams, myCost)
import Algorithm.EqSat.Storage.Backend (SqlBackend(..), SqlValue(..), sqlToInt)
import Algorithm.EqSat.Storage.SQLite (loadGraphResident, loadGraphLazy, saveGraph, flushStore)
import Algorithm.EqSat.Storage.Query (getOrCreateDataset)

import Database.SQLite3 (Database, open, close, exec)

-- | CLI options for the eqsat sub-command.
data EqSatOpts = EqSatOpts
  { eqsatDb         :: String
  , eqsatDataset    :: String
  , eqsatSteps      :: Int
  , eqsatRuleset    :: String
  , eqsatCacheCap   :: Int
  , eqsatBenchmark  :: Bool
  } deriving (Show)

eqsatParser :: Parser EqSatOpts
eqsatParser = EqSatOpts
  <$> strOption
      ( long "db"
      <> metavar "FILE"
      <> help "SQLite database file path" )
  <*> strOption
      ( long "dataset"
      <> metavar "NAME"
      <> help "Dataset name" )
  <*> option auto
      ( long "steps"
      <> value 1
      <> metavar "N"
      <> help "Number of eqsat iterations" )
  <*> strOption
      ( long "ruleset"
      <> value "default"
      <> metavar "RULESET"
      <> help "Rule set: default or params" )
  <*> option auto
      ( long "cache-cap"
      <> value 50000
      <> metavar "N"
      <> help "Resident class cache capacity (default 50000, increase for large graphs)" )
  <*> switch
      ( long "benchmark"
      <> short 'b'
      <> help "Run benchmark comparing in-memory vs paged eqsat" )

-- | Run the eqsat sub-command.
runEqSatCmd :: EqSatOpts -> IO ()
runEqSatCmd EqSatOpts{..} = do
  let rules = case eqsatRuleset of
                "params" -> rewritesParams
                _        -> rewrites

  if eqsatBenchmark
    then runBenchmark EqSatOpts{..} rules
    else runNormal EqSatOpts{..} rules

-- | Normal eqsat run (existing behavior).
runNormal :: EqSatOpts -> [Algorithm.EqSat.Simplify.Rule] -> IO ()
runNormal EqSatOpts{..} rules = do
  putStrLn $ "Loading paged graph from " ++ eqsatDb ++ "..."
  r <- withSQLite eqsatDb $ \db -> do
    dsid <- getOrCreateDataset db eqsatDataset
    totalRows <- queryDb db "SELECT COUNT(*) FROM eclass" []
    let totalBefore = case totalRows of { [[cnt]] -> sqlToInt cnt; _ -> 0 }
    er <- loadGraphLazy db dsid eqsatCacheCap (eqsatCacheCap * 2) (eqsatCacheCap * 2)
    case er of
      Left err -> pure (Left err)
      Right eg -> do
        putStrLn $ "Loaded " ++ show totalBefore ++ " e-classes"
        putStrLn $ "Running " ++ show eqsatSteps ++ " steps of eqsat with '"
                 ++ eqsatRuleset ++ "' rules..."
        let go g = execStateT (runEqSat myCost rules eqsatSteps) g
        eg' <- go eg
        flushStore eg'
        saveResult <- saveGraph db dsid eg'
        case saveResult of
          Left err -> pure (Left ("saveGraph failed: " ++ err))
          Right _  -> do
            case _classStore eg' of
              Nothing -> pure ()
              Just h  -> cpsEndFrontier h
            totalRows' <- queryDb db "SELECT COUNT(*) FROM eclass" []
            let totalAfter = case totalRows' of { [[cnt]] -> sqlToInt cnt; _ -> 0 }
            pure (Right (totalBefore, totalAfter))

  case r of
    Left err -> putStrLn $ "eqsat failed: " ++ err
    Right (before, after) -> do
      putStrLn $ "After eqsat: " ++ show after ++ " e-classes ("
               ++ show (after - before) ++ " change from " ++ show before ++ ")"
      putStrLn $ "Saved to " ++ eqsatDb ++ " [dataset: " ++ eqsatDataset ++ "]"

-- | Benchmark: compare in-memory vs paged eqsat.
runBenchmark :: EqSatOpts -> [Algorithm.EqSat.Simplify.Rule] -> IO ()
runBenchmark EqSatOpts{..} rules = do
  putStrLn $ "=== Benchmark: in-memory vs paged eqsat ==="
  putStrLn $ "Database: " ++ eqsatDb
  putStrLn $ "Dataset: " ++ eqsatDataset
  putStrLn $ "Iterations: " ++ show eqsatSteps
  putStrLn $ "Ruleset: " ++ eqsatRuleset
  putStrLn $ "Cache cap: " ++ show eqsatCacheCap
  putStrLn ""

  withSQLite eqsatDb $ \db -> do
    dsid <- getOrCreateDataset db eqsatDataset
    totalRows <- queryDb db "SELECT COUNT(*) FROM eclass" []
    let totalEclasses = case totalRows of { [[cnt]] -> sqlToInt cnt; _ -> 0 }
    putStrLn $ "Total eclasses in DB: " ++ show totalEclasses
    putStrLn ""

    -- Benchmark 1: In-memory eqsat (loadGraphResident loads all pages, no store handle)
    putStrLn "--- Benchmark 1: In-memory eqsat (loadGraphResident) ---"
    t1_start <- getCPUTime
    r1 <- loadGraphResident db
    case r1 of
      Left err -> putStrLn $ "  loadGraph failed: " ++ err
      Right eg -> do
        let classCount = IntMap.size (_eClass eg)
        putStrLn $ "  Loaded " ++ show classCount ++ " e-classes into memory"
        hFlush stdout
        t1_loaded <- getCPUTime
        let loadTimeMs = fromIntegral (t1_loaded - t1_start) / (1e9 :: Double)
        putStrLn $ "  Load time: " ++ showFF2 loadTimeMs ++ " ms"
        hFlush stdout

        t1_eqsat_start <- getCPUTime
        let go g = execStateT (runEqSat myCost rules eqsatSteps) g
        eg' <- go eg
        t1_eqsat_end <- getCPUTime
        let eqsatTimeMs = fromIntegral (t1_eqsat_end - t1_eqsat_start) / (1e9 :: Double)
            finalClasses = IntMap.size (_eClass eg')
        putStrLn $ "  Eqsat time: " ++ showFF2 eqsatTimeMs ++ " ms"
        putStrLn $ "  Final eclasses: " ++ show finalClasses
        putStrLn ""

    -- Benchmark 2: Paged eqsat (loadGraphLazy, empty resident maps)
    putStrLn "--- Benchmark 2: Paged eqsat (loadGraphLazy) ---"
    t2_start <- getCPUTime
    r2 <- loadGraphLazy db dsid eqsatCacheCap (eqsatCacheCap * 2) (eqsatCacheCap * 2)
    case r2 of
      Left err -> putStrLn $ "  loadGraphLazy failed: " ++ err
      Right eg -> do
        let classCount = IntMap.size (_eClass eg)
        putStrLn $ "  Loaded " ++ show classCount ++ " e-classes (resident cache)"
        hFlush stdout
        t2_loaded <- getCPUTime
        let loadTimeMs = fromIntegral (t2_loaded - t2_start) / (1e9 :: Double)
        putStrLn $ "  Load time: " ++ showFF2 loadTimeMs ++ " ms"
        hFlush stdout

        t2_eqsat_start <- getCPUTime
        let go g = execStateT (runEqSat myCost rules eqsatSteps) g
        eg' <- go eg
        t2_eqsat_end <- getCPUTime
        let eqsatTimeMs = fromIntegral (t2_eqsat_end - t2_eqsat_start) / (1e9 :: Double)
            finalClasses = IntMap.size (_eClass eg')
        putStrLn $ "  Eqsat time: " ++ showFF2 eqsatTimeMs ++ " ms"
        putStrLn $ "  Final eclasses: " ++ show finalClasses
        putStrLn ""

    putStrLn "=== Benchmark complete ==="

showFF2 :: Double -> String
showFF2 x = show (fromIntegral (round (x * 100) :: Int) / 100 :: Double)

-- | Open a SQLite database, run an action, and close it.
withSQLite :: String -> (Database -> IO a) -> IO a
withSQLite path f = bracket openDb close f
  where
    openDb = do
      db <- open (T.pack path)
      exec db "PRAGMA journal_mode=WAL"
      pure db
