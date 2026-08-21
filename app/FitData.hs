{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module FitData
  ( FitDataOpts(..)
  , fitdataParser
  , runFitData
  ) where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (mapConcurrently_)
import Control.Monad (replicateM, when, unless, void)
import Control.Exception (bracket)
import Data.IORef
import Data.List (maximumBy, foldl')
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
import Algorithm.EqSat.Storage.Types (serializeTheta)
import Database.SQLite3 (Database, open, close)

-- | CLI options for the fitdata sub-command.
data FitDataOpts = FitDataOpts
  { fitdataDb        :: String
  , fitdataDataset   :: String
  , fitdataData      :: String
  , fitdataLoss      :: Loss
  , fitdataHasHeader :: Bool
  , fitdataNRep      :: Int
  , fitdataNIter     :: Int
  , fitdataBatchSize :: Int
  , fitdataQuiet     :: Bool
  } deriving (Show)

fitdataParser :: Parser FitDataOpts
fitdataParser = FitDataOpts
  <$> strOption
      ( long "db"
      <> metavar "FILE"
      <> help "SQLite database file path" )
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

  putStrLn $ "Opening " ++ fitdataDb ++ "..."
  hFlush stdout
  withSQLite fitdataDb $ \db -> do
    dsid <- getOrCreateDataset db fitdataDataset
    unfitted <- queryUnfitted db dsid
    let total = length unfitted
    putStrLn $ "Found " ++ show total ++ " unfitted e-classes"
    hFlush stdout

    if total == 0
      then putStrLn "Nothing to fit."
      else do
        nCaps <- getNumCapabilities
        counter <- newIORef (0 :: Int)
        fittedSet <- newIORef IntSet.empty
        cacheRef <- newIORef IntMap.empty

        let processBatch [] = pure ()
            processBatch batch = do
              cache0 <- readIORef cacheRef
              -- Phase 0: ensure batch root eclasses are in the cache
              let batchIds = IntSet.fromList batch
                  batchMissing = IntSet.toList (IntSet.difference batchIds (IntSet.fromList (IntMap.keys cache0)))
              when (not (null batchMissing)) $ do
                newPages <- loadPagesBulk db batchMissing
                writeIORef cacheRef (cache0 `IntMap.union` newPages)
              cache1 <- readIORef cacheRef

              -- Phase 1: expand batch to include all reachable sub-classes
              let needed = foldl' (\s eid -> s `IntSet.union` expandTreeIds cache1 eid) IntSet.empty batch
                  missing = IntSet.toList (IntSet.difference needed (IntSet.fromList (IntMap.keys cache1)))
              when (not (null missing)) $ do
                putStrLn $ "  Loading " ++ show (length missing) ++ " sub-expression pages..."
                hFlush stdout
                newPages <- loadPagesBulk db missing
                writeIORef cacheRef (cache1 `IntMap.union` newPages)

              cache <- readIORef cacheRef
              fittedNow <- readIORef fittedSet

              -- Phase 2: fit all unfitted classes in the expanded set
              let toFit = filter (\eid -> not (IntSet.member eid fittedNow)) (IntSet.toList needed)
              setMTPopParallel True
              let chunks = chunk nCaps toFit
              void $ mapConcurrently_ (mapM_ (fitOne fitdataQuiet db dsid cache xTrain yTrain mYErr fitdataLoss fitdataNIter fitdataNRep nNoiseParams counter fittedSet)) chunks
              setMTPopParallel False
              unless fitdataQuiet $ putStrLn "  [checkpoint] committed batch"
              fitted <- readIORef counter
              when (fitted `mod` 100000 == 0) $ putStrLn ("fitted " ++ show fitted ++ " so far.")

        let batches = chunk fitdataBatchSize unfitted
        mapM_ processBatch batches

        fitted <- readIORef counter
        putStrLn $ "Fitted " ++ show fitted ++ "/" ++ show total
                 ++ " expressions"

-- | Fit a single e-class from the preloaded cache.
fitOne :: SqlBackend db
       => Bool -> db -> Int -> IntMap.IntMap EClass
       -> [VU.Vector Double] -> VU.Vector Double -> Maybe (VU.Vector Double)
       -> Loss -> Int -> Int -> Int -> IORef Int -> IORef IntSet.IntSet -> EClassId -> IO ()
fitOne quiet db dsid cache xTrain yTrain mYErr loss nIter nRep nNoiseParams counter fittedSet eid = do
  -- Skip if already fitted (by a previous sub-expression fit in this batch)
  alreadyFitted <- IntSet.member eid <$> readIORef fittedSet
  if alreadyFitted
    then pure ()
    else do
      let mTree = reconstructFromCache cache eid
      case mTree of
        Nothing -> do
          unless quiet $ putStrLn $ "  eclass " ++ show eid ++ ": not in cache (skipped)"
          modifyIORef' counter (+1)
        Just tree -> do
          let !tree' = relabelParams tree
              !np = countParamsUniq tree' + nNoiseParams
              !sz = countNodes tree'
          if np == 0
            then do
              -- Analytical fit: no parameters, compute loss directly
              let fitness = analyticalFit loss xTrain yTrain tree'
              writeDatasetFit db dsid eid (Just fitness) Nothing
                (T.pack (serializeTheta [])) sz
              modifyIORef' counter (+1)
              modifyIORef' fittedSet (IntSet.insert eid)
              unless quiet $ putStrLn $ "  eclass " ++ show eid ++ " (" ++ takeExpr tree' ++ "): fitness=" ++ showFit fitness ++ " [analytical]"
            else do
              let funAndGrad = compileLossAndGrad MultiThread loss mYErr xTrain yTrain tree'
                  runRestart = do
                    theta0 <- VU.replicateM np (randomRIO (-1, 1))
                    let (theta, lossVal, _) = minimizeNLLWith funAndGrad VAR1 nIter theta0
                    pure (negate lossVal, theta)
              results <- replicateM nRep runRestart
              let (bestFitness, bestTheta) = maximumBy (comparing fst) results
              writeDatasetFit db dsid eid (Just bestFitness) Nothing
                (T.pack (serializeTheta [bestTheta])) sz
              modifyIORef' counter (+1)
              modifyIORef' fittedSet (IntSet.insert eid)
              unless quiet $ putStrLn $ "  eclass " ++ show eid ++ " (" ++ takeExpr tree' ++ "): fitness=" ++ showFit bestFitness

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

-- | Query for e-class IDs that are not yet fitted for a dataset.
queryUnfitted :: SqlBackend db => db -> Int -> IO [EClassId]
queryUnfitted db dsid = do
  rows <- queryDb db
    "SELECT e.eid FROM eclass e \
    \LEFT JOIN dataset_fit df ON df.eid = e.eid AND df.dataset_id = ? \
    \WHERE df.fitted IS NULL OR df.fitted = 0"
    [SqlInteger (fromIntegral dsid)]
  pure [ sqlToInt eid | [eid] <- rows ]

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
withSQLite path = bracket (open (T.pack path)) close
