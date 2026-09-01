{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FitData
  ( FitDataOpts(..)
  , fitdataParser
  , runFitData
  , runRefit
  ) where

import Control.Concurrent (getNumCapabilities, threadDelay)
import Control.Concurrent.Async (mapConcurrently_, mapConcurrently, async, wait)
import Control.Monad (replicateM, when, unless, void, forM_)
import Control.Exception (bracket, SomeException, catch, SomeAsyncException(..))
import Data.IORef
import Data.List (maximumBy, foldl', sortBy)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as VU
import Options.Applicative hiding (Const)
import System.IO (hPutStrLn, hFlush, stdout, stderr)
import System.Random (randomRIO)

import Data.SRTree (Fix(..), SRTree(..), Op(..), relabelParams, countParamsUniq, countNodes)
import Data.SRTree.Print (showExpr)
import Data.SRTree.Eval (Target, compile)
import Data.SRTree.Datasets (loadDataset)
import Algorithm.SRTree.NonlinearOpt (compileLossAndGrad, minimizeNLLWith)
import Algorithm.SRTree.Likelihoods (Loss(..), Distribution(..))
import Algorithm.SRTree.AD (ADBackEnd(..))
import Algorithm.SRTree.AD.Unboxed (setMTPopParallel)
import Numeric.Optimization.NLOPT (LocalAlgorithm(..))
import Algorithm.EqSat.Egraph (EClassId, EClass(..))
import Algorithm.EqSat.Storage.Backend (SqlBackend(..), SqlValue(..), sqlToInt)
import Algorithm.EqSat.Storage.Extract (reconstructFromCache, expandTreeIds)
import Algorithm.EqSat.Storage.SQLite (loadPagesBulk)
import Algorithm.EqSat.Storage.Query (getOrCreateDataset, writeDatasetFit)
import Algorithm.EqSat.Storage.Schema (createSchemaFit)
import Algorithm.EqSat.Storage.Types (serializeTheta)
import Database.SQLite3 (Database, open, close, exec)

-- | CLI options for the fitdata sub-command.
data FitDataOpts = FitDataOpts
  { fitdataEgraph      :: String
  , fitdataFitdb       :: String
  , fitdataDataset     :: String
  , fitdataData        :: String
  , fitdataLoss        :: Loss
  , fitdataHasHeader   :: Bool
  , fitdataNRep        :: Int
  , fitdataNIter       :: Int
  , fitdataBatchSize   :: Int
  , fitdataQuiet       :: Bool
  } deriving (Show)

fitdataParser :: Parser FitDataOpts
fitdataParser = FitDataOpts
  <$> strOption
      ( long "egraph"
      <> metavar "FILE"
      <> help "Path to e-graph database" )
  <*> strOption
      ( long "fitdb"
      <> metavar "FILE"
      <> help "Path to fit database" )
  <*> strOption
      ( long "dataset"
      <> metavar "NAME"
      <> help "Dataset name" )
  <*> strOption
      ( long "data"
      <> metavar "SPEC"
      <> help "Dataset CSV spec: file:start:end:target:features:yerr" )
  <*> option auto
      ( long "loss"
      <> value (NLL Gaussian)
      <> metavar "LOSS"
      <> help "Loss function (MSE, NLL Gaussian, etc.)" )
  <*> flag True False
      ( long "no-header"
      <> help "CSV has no header row (default: has header)" )
  <*> option auto
      ( long "n-rep"
      <> value 1
      <> metavar "N"
      <> help "Number of random restarts per expression" )
  <*> option auto
      ( long "n-iter"
      <> value 30
      <> metavar "N"
      <> help "Max NLopt iterations" )
  <*> option auto
      ( long "batch-size"
      <> value 10000
      <> metavar "N"
      <> help "Fit N expressions per batch" )
  <*> switch
      ( long "quiet"
      <> short 'q'
      <> help "Suppress per-expression output; print progress every 10k expressions" )

-- | Run the fitdata sub-command.
runFitData :: FitDataOpts -> IO ()
runFitData opts = do
  let FitDataOpts{..} = opts
  putStrLn $ "Loading dataset: " ++ fitdataData
  hFlush stdout
  ((xTrain, yTrain, _xVal, _yVal), (mYErr, _), _varnames, _target) <-
    loadDataset fitdataData fitdataHasHeader

  let nNoiseParams = case fitdataLoss of
        NLL Gaussian -> 1
        NLL ROXY     -> 3
        _            -> 0

  putStrLn $ "Opening egraph: " ++ fitdataEgraph ++ "..."
  putStrLn $ "Opening fitdb: " ++ fitdataFitdb ++ "..."
  hFlush stdout
  withSQLite fitdataFitdb $ \fitDb -> do
    createSchemaFit fitDb
    withSQLite fitdataEgraph $ \egDb -> do
      dsid <- getOrCreateDataset fitDb fitdataDataset

      total <- countUnfitted egDb fitDb dsid
      putStrLn $ "Found " ++ show total ++ " unfitted e-classes"
      hFlush stdout

      if total == 0
        then putStrLn "Nothing to fit."
        else do
          nCaps <- getNumCapabilities
          counter <- newIORef (0 :: Int)
          nanSet <- newIORef IntSet.empty
          nanCount <- newIORef (0 :: Int)

          -- Phase 0-1: load pages + expand (reads from egraph DB)
          -- Accumulates invalid/analytical fits in an IORef (no DB writes)
          let loadPhase pendingRef batch = do
                let batchIds = IntSet.fromList batch
                batchPages <- loadPagesBulk egDb (IntSet.toList batchIds)
                let !cache0 = batchPages
                let expandLoop !cache = do
                      let needed = foldl' (\s eid -> s `IntSet.union` expandTreeIds cache eid) IntSet.empty batch
                          missing = IntSet.toList (IntSet.difference needed (IntSet.fromList (IntMap.keys cache)))
                      if null missing
                        then pure (cache, needed)
                        else do
                          putStrLn $ "  Loading " ++ show (length missing) ++ " sub-expression pages..."
                          hFlush stdout
                          newPages <- loadPagesBulk egDb missing
                          expandLoop (cache `IntMap.union` newPages)
                (cache1, needed) <- expandLoop cache0

                -- Phase 2: build jobs (reconstruct, handle cache misses)
                let toFit = IntSet.toList needed
                mjobs <- mapM (buildJobNoWrite cache1 nNoiseParams counter pendingRef) toFit
                let jobs = sortBy (comparing jobSize) (catMaybes mjobs)

                -- Phase 3: classify bottom-up, pruning NaN, collecting survivors
                survivorRef <- newIORef ([] :: [FitJob])
                beforeNan <- readIORef nanCount
                mapM_ (classifyNoWrite fitdataQuiet nanSet counter nanCount survivorRef pendingRef dsid cache1 xTrain yTrain mYErr fitdataLoss nNoiseParams) jobs
                survivors <- readIORef survivorRef
                afterNan <- readIORef nanCount
                when (afterNan > beforeNan) $
                  unless fitdataQuiet $ do
                    putStrLn $ "  +" ++ show (afterNan - beforeNan)
                      ++ " eclasses inserted with NaN this batch (total " ++ show afterNan ++ ")"
                    hFlush stdout
                pure survivors

          -- Phase 4-5: parallel NLopt + batch write (uses fitDb)
          let fitPhase pendingRef survivors = do
                let chunks = chunk nCaps survivors
                setMTPopParallel False
                results <- fmap concat $ mapConcurrently (mapM (fitOneNLopt fitdataQuiet xTrain yTrain mYErr fitdataLoss fitdataNIter fitdataNRep counter)) chunks
                setMTPopParallel True
                -- Batch write all pending fits (invalid + analytical + NLopt)
                pending <- atomicModifyIORef' pendingRef (\ps -> ([], ps))
                execDb fitDb "BEGIN"
                forM_ pending $ \(FitPending eid fit theta sz) ->
                  case fit of
                    Nothing -> writeInvalidFit fitDb dsid eid  -- invalid: fitted=1, fitness=NULL
                    Just f -> writeDatasetFit fitDb dsid eid (Just f) Nothing theta sz
                forM_ results $ \(FitResult eid fit theta sz) ->
                  writeDatasetFit fitDb dsid eid (Just fit) Nothing theta sz
                execDb fitDb "COMMIT"
                unless fitdataQuiet $ putStrLn "  [checkpoint] committed batch"

          -- Collect all batches, then process with pipelining
          allBatches <- collectBatches egDb fitDb dsid fitdataBatchSize
          pendingRef <- newIORef ([] :: [FitPending])
          processBatchesPipelined egDb fitDb nCaps (loadPhase pendingRef) (fitPhase pendingRef) allBatches

          fitted <- readIORef counter
          putStrLn $ "Fitted " ++ show fitted ++ "/" ++ show total
                   ++ " expressions"

-- | A unit of NLopt work: a reconstructed, relabeled expression with its
-- parameter count and node size precomputed.
data FitJob = FitJob
  { jobEid   :: !EClassId
  , jobTree  :: !(Fix SRTree)
  , jobFree  :: !Bool        -- ^ structurally parameter-free (no Param nodes)
  , jobNp    :: !Int         -- ^ total free params incl. loss noise params
  , jobSize  :: !Int
  }

-- | Result of fitting one expression (pure data, no DB side effects).
data FitResult = FitResult
  { frEid     :: !EClassId
  , frFitness :: !Double
  , frTheta   :: !T.Text
  , frSize    :: !Int
  }

-- | A pending DB write (accumulated during loadPhase, written in fitPhase).
data FitPending = FitPending
  { fpEid     :: !EClassId
  , fpFitness :: !(Maybe Double)
  , fpTheta   :: !T.Text
  , fpSize    :: !Int
  }

-- | Reconstruct a job, accumulating invalid writes instead of writing immediately.
buildJobNoWrite :: IntMap.IntMap EClass -> Int -> IORef Int -> IORef [FitPending]
                -> EClassId -> IO (Maybe FitJob)
buildJobNoWrite cache nNoiseParams counter pendingRef eid = do
  case reconstructFromCache cache eid of
    Nothing -> do
      atomicModifyIORef' pendingRef (\ps -> (FitPending eid Nothing (T.pack (serializeTheta [])) 0 : ps, ()))
      atomicModifyIORef' counter (\n -> let !n' = n + 1 in (n', ()))
      pure Nothing
    Just tree -> do
      let !tree' = relabelParams tree
          !nup   = countParamsUniq tree'
          !np    = nup + nNoiseParams
          !free  = nup == 0
          !sz    = countNodes tree'
      pure (Just (FitJob eid tree' free np sz))

-- | Classify without writing to DB. Accumulates pending writes.
classifyNoWrite :: Bool -> IORef IntSet.IntSet -> IORef Int -> IORef Int
                -> IORef [FitJob] -> IORef [FitPending]
                -> Int -> IntMap.IntMap EClass
                -> [VU.Vector Double] -> VU.Vector Double -> Maybe (VU.Vector Double)
                -> Loss -> Int -> FitJob -> IO ()
classifyNoWrite quiet nanSet counter nanCount survivorRef pendingRef dsid cache xTrain yTrain mYErr loss nNoiseParams (FitJob eid tree' free np sz) = do
  let desc = expandTreeIds cache eid
  nan <- readIORef nanSet
  if not (IntSet.null (IntSet.intersection desc nan))
    then pruneNoWrite
    else if not free
      then survivor
      else do
        let fitness = analyticalFit loss xTrain yTrain tree'
        if isInvalid fitness
          then pruneNoWrite
          else if np == 0
            then do
              atomicModifyIORef' pendingRef (\ps -> (FitPending eid (Just fitness) (T.pack (serializeTheta [])) sz : ps, ()))
              atomicModifyIORef' counter (\n -> let !n' = n + 1 in (n', ()))
              unless quiet $ putStrLn $ "  eclass " ++ show eid ++ " (" ++ takeExpr tree' ++ "): fitness=" ++ showFit fitness ++ " [analytical]"
            else survivor
  where
    pruneNoWrite = do
      atomicModifyIORef' pendingRef (\ps -> (FitPending eid Nothing (T.pack (serializeTheta [])) 0 : ps, ()))
      atomicModifyIORef' nanSet (\s -> (IntSet.insert eid s, ()))
      atomicModifyIORef' nanCount (\n -> let !n' = n + 1 in (n', ()))
      atomicModifyIORef' counter (\n -> let !n' = n + 1 in (n', ()))
    survivor = do
      atomicModifyIORef' survivorRef (\xs -> (FitJob eid tree' free np sz : xs, ()))

-- | Classify a single eclass (run in bottom-up order). If the expression
-- contains any known-NaN subexpression, or is a parameter-less expression that
-- evaluates to NaN/infinity, it is pruned (written as a NULL-fitness fitted row)
-- and recorded in @nanSet@ so its ancestors propagate. Otherwise it is queued as
-- an NLopt survivor.
classify :: SqlBackend db
         => Bool -> IORef IntSet.IntSet -> IORef Int -> IORef Int -> IORef [FitJob]
         -> db -> Int -> IntMap.IntMap EClass
         -> [VU.Vector Double] -> VU.Vector Double -> Maybe (VU.Vector Double)
         -> Loss -> Int -> FitJob -> IO ()
classify quiet nanSet counter nanCount survivorRef db dsid cache xTrain yTrain mYErr loss nNoiseParams (FitJob eid tree' free np sz) = do
  let desc = expandTreeIds cache eid
  nan <- readIORef nanSet
  if not (IntSet.null (IntSet.intersection desc nan))
    then prune
    else if not free
      then survivor
      else do
        -- Structurally parameter-free: evaluate analytically to decide NaN.
        -- Handles the noise-param loss case (e.g. NLL Gaussian, np = nNoiseParams).
        let fitness = analyticalFit loss xTrain yTrain tree'
        if isInvalid fitness
          then prune
          else if np == 0
            then do
              -- Fully parameter-less: analytic fit is exact.
              writeDatasetFit db dsid eid (Just fitness) Nothing
                (T.pack (serializeTheta [])) sz
              atomicModifyIORef' counter (\n -> let !n' = n + 1 in (n', ()))
              unless quiet $ putStrLn $ "  eclass " ++ show eid ++ " (" ++ takeExpr tree' ++ "): fitness=" ++ showFit fitness ++ " [analytical]"
            else survivor
  where
    -- An eclass doomed to NaN (contains a known-NaN subexpr, or is itself NaN):
    -- write a NULL-fitness fitted row, record it, and count it.
    prune = do
      writeInvalidFit db dsid eid
      atomicModifyIORef' nanSet (\s -> (IntSet.insert eid s, ()))
      atomicModifyIORef' nanCount (\n -> let !n' = n + 1 in (n', ()))
      atomicModifyIORef' counter (\n -> let !n' = n + 1 in (n', ()))
    survivor = do
      atomicModifyIORef' survivorRef (\xs -> (FitJob eid tree' free np sz : xs, ()))

-- | Fit one expression via NLopt. Only called on survivors that are not
-- statically NaN. Runs @nRep@ restarts and keeps the best.
-- Writes result to DB immediately.
fitOne :: SqlBackend db
       => Bool -> db -> Int
       -> [VU.Vector Double] -> VU.Vector Double -> Maybe (VU.Vector Double)
       -> Loss -> Int -> Int -> IORef Int -> FitJob -> IO ()
fitOne quiet db dsid xTrain yTrain mYErr loss nIter nRep counter job = do
  fr <- fitOneNLopt quiet xTrain yTrain mYErr loss nIter nRep counter job
  writeDatasetFit db dsid (frEid fr) (Just (frFitness fr)) Nothing (frTheta fr) (frSize fr)

-- | Pure NLopt fit (no DB side effects). Returns a FitResult.
fitOneNLopt :: Bool
            -> [VU.Vector Double] -> VU.Vector Double -> Maybe (VU.Vector Double)
            -> Loss -> Int -> Int -> IORef Int -> FitJob -> IO FitResult
fitOneNLopt quiet xTrain yTrain mYErr loss nIter nRep counter (FitJob eid tree' _ np sz) = do
  let funAndGrad = compileLossAndGrad MultiThread loss mYErr xTrain yTrain tree'
      runRestart = do
        theta0 <- VU.replicateM np (randomRIO (-1, 1))
        let (theta, lossVal, _) = minimizeNLLWith funAndGrad VAR1 nIter theta0
        pure (negate lossVal, theta)
  results <- replicateM nRep runRestart
  let (bestFitness, bestTheta) = maximumBy (comparing fst) results
      !thetaText = T.pack (serializeTheta [bestTheta])
  atomicModifyIORef' counter (\n -> let !n' = n + 1 in (n', ()))
  unless quiet $ putStrLn $ "  eclass " ++ show eid ++ " (" ++ takeExpr tree' ++ "): fitness=" ++ showFit bestFitness
  pure (FitResult eid bestFitness thetaText sz)

-- | Whether a fitness value is unusable (NaN or +/-Infinity), so it can be
-- pruned and propagated to ancestors.
isInvalid :: Double -> Bool
isInvalid f = isNaN f || isInfinite f

-- | Write a fitted row with NULL fitness (used for pruned / cache-miss eclasses).
-- Marks @fitted = 1@ so the eclass is removed from the unfitted queue but stays
-- distinguishable from rows with a real fitness.
writeInvalidFit :: SqlBackend db => db -> Int -> EClassId -> IO ()
writeInvalidFit db dsid eid =
  runDb db
    "INSERT INTO dataset_fit (dataset_id, eid, fitness, dl, theta, size, evaluated, fitted) \
    \VALUES (?, ?, NULL, NULL, '', 0, 0, 1) \
    \ON CONFLICT (dataset_id, eid) DO UPDATE SET \
    \fitness = NULL, dl = NULL, theta = '', size = 0, evaluated = 0, fitted = 1"
    [SqlInteger (fromIntegral dsid), SqlInteger (fromIntegral eid)]

-- | Analytical fitness for parameter-free expressions.
-- No NLopt needed — compute loss directly.
analyticalFit :: Loss -> [VU.Vector Double] -> VU.Vector Double -> Fix SRTree -> Double
analyticalFit loss xTrain yTrain tree =
  let preds = compile xTrain tree VU.empty  -- evaluate with empty theta
      m = fromIntegral (VU.length yTrain) :: Double
      residuals = VU.zipWith (-) preds yTrain
  in case loss of
    NLL Gaussian ->
      let mse = VU.sum (VU.map (\r -> r * r) residuals) / m
          sigma2 = mse
          nll = negate (m / 2 * log (2 * pi * sigma2) + m / 2)
      in if isNaN nll || isInfinite nll then -(1/0) else nll
    MSE ->
      let mse = VU.sum (VU.map (\r -> r * r) residuals) / m
      in negate mse
    _ ->
      let mse = VU.sum (VU.map (\r -> r * r) residuals) / m
      in negate mse  -- fallback: use MSE

-- | Count unfitted e-classes for a dataset (cross-DB: egraph + fit).
countUnfitted :: (SqlBackend db1, SqlBackend db2) => db1 -> db2 -> Int -> IO Int
countUnfitted egDb fitDb dsid = do
  -- Total eclasses in egraph
  totalRows <- queryDb egDb "SELECT COUNT(*) FROM eclass" []
  let totalEclasses = case totalRows of { [[cnt]] -> sqlToInt cnt; _ -> 0 }
  -- Fitted eclasses in fit DB
  fittedRows <- queryDb fitDb
    "SELECT COUNT(*) FROM dataset_fit WHERE dataset_id = ? AND fitted = 1"
    [SqlInteger (fromIntegral dsid)]
  let totalFitted = case fittedRows of { [[cnt]] -> sqlToInt cnt; _ -> 0 }
  pure (totalEclasses - totalFitted)

-- | Load all fitted eclass IDs for a dataset into an IntSet.
loadFittedSet :: SqlBackend db => db -> Int -> IO IntSet.IntSet
loadFittedSet fitDb dsid = do
  rows <- queryDb fitDb
    "SELECT eid FROM dataset_fit WHERE dataset_id = ? AND fitted = 1"
    [SqlInteger (fromIntegral dsid)]
  pure $ IntSet.fromList [ sqlToInt eid | [eid] <- rows ]

-- | Stream unfitted e-class IDs in batches, processing each batch
-- without materializing the full ID list in memory.
-- Cross-DB: streams from egraph, filters against fit DB.
processStreamingBatches :: (SqlBackend db1, SqlBackend db2)
                        => db1 -> db2 -> Int -> Int -> ([EClassId] -> IO ()) -> IO ()
processStreamingBatches egDb fitDb dsid batchSize processBatch = do
  fittedSet <- loadFittedSet fitDb dsid
  batchRef <- newIORef ([] :: [EClassId])
  countRef <- newIORef (0 :: Int)
  foldQueryDb egDb
    "SELECT eid FROM eclass ORDER BY eid"
    []
    ()
    (\() cols -> case cols of
      [eidCol] -> do
        let !eid = sqlToInt eidCol
        if IntSet.member eid fittedSet
          then pure ()
          else do
            batch <- readIORef batchRef
            let !batch' = eid : batch
            n <- readIORef countRef
            let !n' = n + 1
            writeIORef countRef n'
            if n' >= batchSize
              then do
                processBatch (reverse batch')
                writeIORef batchRef []
                writeIORef countRef 0
              else writeIORef batchRef batch'
        pure ()
      _ -> pure ())
  remaining <- readIORef batchRef
  when (not (null remaining)) $ processBatch (reverse remaining)

-- | Collect all unfitted batch IDs into a list of batches.
collectBatches :: (SqlBackend db1, SqlBackend db2)
               => db1 -> db2 -> Int -> Int -> IO [[EClassId]]
collectBatches egDb fitDb dsid batchSize = do
  fittedSet <- loadFittedSet fitDb dsid
  batchesRef <- newIORef ([] :: [[EClassId]])
  batchRef <- newIORef ([] :: [EClassId])
  countRef <- newIORef (0 :: Int)
  foldQueryDb egDb
    "SELECT eid FROM eclass ORDER BY eid"
    []
    ()
    (\() cols -> case cols of
      [eidCol] -> do
        let !eid = sqlToInt eidCol
        if IntSet.member eid fittedSet
          then pure ()
          else do
            batch <- readIORef batchRef
            let !batch' = eid : batch
            n <- readIORef countRef
            let !n' = n + 1
            writeIORef countRef n'
            if n' >= batchSize
              then do
                modifyIORef' batchesRef (reverse batch' :)
                writeIORef batchRef []
                writeIORef countRef 0
              else writeIORef batchRef batch'
        pure ()
      _ -> pure ())
  remaining <- readIORef batchRef
  when (not (null remaining)) $ modifyIORef' batchesRef (reverse remaining :)
  allBatches <- readIORef batchesRef
  pure (reverse allBatches)

-- | Process batches with pipelining: while batch K's Phase 4 (NLopt) runs,
-- batch K+1's Phase 0-1 (loading) starts in the background.
-- Hides I/O latency behind compute.
processBatchesPipelined :: (SqlBackend db1, SqlBackend db2)
                        => db1 -> db2 -> Int
                        -> ([EClassId] -> IO [FitJob])
                        -> ([FitJob] -> IO ())
                        -> [[EClassId]]
                        -> IO ()
processBatchesPipelined _ _ _ _ _ [] = pure ()
processBatchesPipelined egDb fitDb nCaps loadPhase fitPhase [batch] = do
  survivors <- loadPhase batch
  fitPhase survivors
processBatchesPipelined egDb fitDb nCaps loadPhase fitPhase (batch:rest) = do
  -- Start loading next batch in background
  nextLoad <- async $ loadPhase (head rest)
  -- Load and fit current batch
  survivors <- loadPhase batch
  fitPhase survivors
  -- Wait for next load, then continue
  processBatchesPipelined' nextLoad rest
  where
    processBatchesPipelined' prevLoad [] = do
      survivors <- wait prevLoad
      fitPhase survivors
    processBatchesPipelined' prevLoad [b] = do
      survivors <- wait prevLoad
      fitPhase survivors
    processBatchesPipelined' prevLoad (b:bs) = do
      nextLoad <- async $ loadPhase b
      survivors <- wait prevLoad
      fitPhase survivors
      processBatchesPipelined' nextLoad bs

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = let (h, t) = splitAt n xs in h : chunk n t

takeExpr :: Fix SRTree -> String
takeExpr t
  | length s > 40 = take 40 s ++ "..."
  | otherwise = s
  where s = showExpr t

showFit :: Double -> String
showFit f
  | f == (-1/0) = "-Infinity"
  | f == (1/0)  = "Infinity"
  | isNaN f     = "NaN"
  | otherwise   = show (fromIntegral (round (f * 1000) :: Int) / 1000 :: Double)

withSQLite :: String -> (Database -> IO a) -> IO a
withSQLite path f = bracket openDb close f
  where
    openDb = do
      db <- open (T.pack path)
      exec db "PRAGMA journal_mode=DELETE"
      exec db "PRAGMA busy_timeout = 30000"
      pure db

-- | Run refit: clear all fitted data for a dataset, then re-fit everything.
runRefit :: FitDataOpts -> IO ()
runRefit opts = do
  let FitDataOpts{..} = opts
  putStrLn $ "Refitting dataset: " ++ fitdataDataset
  putStrLn $ "Clearing previous fit data..."
  hFlush stdout
  withSQLite fitdataFitdb $ \fitDb -> do
    createSchemaFit fitDb
    dsid <- getOrCreateDataset fitDb fitdataDataset
    runDb fitDb "DELETE FROM dataset_fit WHERE dataset_id = ?" [SqlInteger (fromIntegral dsid)]
    putStrLn $ "Cleared fit data for dataset " ++ show dsid ++ "."
    hFlush stdout
  -- Now run the normal fitdata flow
  runFitData opts
