{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Ingest
  ( IngestOpts(..)
  , ingestParser
  , runIngest
  ) where

import Control.Exception (bracket, SomeException, catch, displayException)
import Control.Monad (forM_, when, unless)
import Data.IORef
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as VU
import Options.Applicative
import System.IO (hIsEOF, hGetLine, stdin, openFile, IOMode(..), hClose, hPutStrLn, stderr, hFlush)
import System.Exit (exitFailure)
import System.Random (randomRIO)

import Data.SRTree (Fix(..), SRTree(..), relabelParams, countParamsUniq)
import Data.SRTree.Eval (Target)
import Data.SRTree.Datasets (loadDataset)
import Text.ParseSR (SRAlgs(..), parseSR)
import Algorithm.SRTree.NonlinearOpt (minimizeNLL')
import Algorithm.SRTree.Likelihoods (Loss(..), Distribution(..))
import Algorithm.SRTree.AD (ADBackEnd(..))
import Numeric.Optimization.NLOPT (LocalAlgorithm(..))
import Algorithm.EqSat.Storage.Import (importEqs, importEqsInit, ImportSummary(..))
import Algorithm.EqSat.Storage.SQLite ()
import Database.SQLite3 (Database, open, close, exec)

-- | CLI options for the ingest sub-command.
data IngestOpts = IngestOpts
  { ingestDb         :: String
  , ingestExprs      :: String
  , ingestDataset    :: String
  , ingestFormat     :: SRAlgs
  , ingestVarnames   :: String
  , ingestData       :: String
  , ingestFit        :: Bool
  , ingestLoss       :: Loss
  , ingestEqsatSteps :: Int
  , ingestReparam    :: Bool
  , ingestHasHeader  :: Bool
  , ingestQuiet      :: Bool
  } deriving (Show)

ingestParser :: Parser IngestOpts
ingestParser = IngestOpts
  <$> strOption
      ( long "db"
      <> metavar "FILE"
      <> help "SQLite database file path" )
  <*> strOption
      ( long "expressions"
      <> value ""
      <> metavar "FILE"
      <> help "File with one expression per line (empty = stdin)" )
  <*> strOption
      ( long "dataset"
      <> value ""
      <> metavar "NAME"
      <> help "Dataset name (required for fitting)" )
  <*> option auto
      ( long "format"
      <> value OPERON
      <> metavar "FORMAT"
      <> help "Expression format: OPERON, TIR, HL, BINGO, GOMEA, PYSR" )
  <*> strOption
      ( long "varnames"
      <> value "x0,x1,x2,x3,x4,x5"
      <> metavar "VARNAMES"
      <> help "Comma-separated variable names" )
  <*> strOption
      ( long "data"
      <> value ""
      <> metavar "SPEC"
      <> help "Dataset CSV spec: file:start:end:target:features:yerr" )
  <*> switch
      ( long "fit"
      <> help "Fit expressions after ingest" )
  <*> option auto
      ( long "loss"
      <> value (NLL Gaussian)
      <> metavar "LOSS"
      <> help "Loss function (MSE, NLL Gaussian, etc.)" )
  <*> option auto
      ( long "eqsat-steps"
      <> value 0
      <> metavar "N"
      <> help "Run N eqsat steps after ingest" )
  <*> switch
      ( long "reparam"
      <> help "Float constants to parameters" )
  <*> switch
      ( long "has-header"
      <> help "CSV has header row (default: True)" )
  <*> switch
      ( long "quiet"
      <> short 'q'
      <> help "Suppress per-expression output; print progress every 10k expressions" )

-- | Run the ingest sub-command.
runIngest :: IngestOpts -> IO ()
runIngest IngestOpts{..} = do
  -- Validate: --dataset is required when --fit is used
  when (ingestFit && null ingestDataset) $ do
    hPutStrLn stderr "Error: --dataset is required when --fit is used"
    exitFailure

  let mds = if null ingestDataset then Nothing else Just ingestDataset
      alg = ingestFormat
      varnames = ingestVarnames
      batchSize = 1000 :: Int
      progressInterval = if ingestQuiet then 10000 else batchSize

  -- Open the expression file (or stdin)
  h <- if null ingestExprs then pure stdin else openFile ingestExprs ReadMode

  -- Open DB
  putStrLn $ "Opening " ++ ingestDb ++ "..."
  db <- open (T.pack ingestDb)
  exec db "PRAGMA journal_mode=WAL"

  -- One-time schema + index setup (skipped by repeated importEqs calls)
  importEqsInit db

  -- Process line by line, batch and insert
  putStrLn "Processing expressions..."
  totalRef   <- newIORef (0 :: Int)
  validRef   <- newIORef (0 :: Int)
  failedRef  <- newIORef (0 :: Int)
  classesRef <- newIORef (0 :: Int)
  batchRef   <- newIORef ([] :: [(Fix SRTree, [Target], Maybe Double)])

  let flushBatch = do
        batch <- readIORef batchRef
        unless (null batch) $ do
          r <- importEqs db mds (reverse batch)
          case r of
            Left err -> hPutStrLn stderr $ "  BATCH INSERT FAILED: " ++ err
            Right s  -> modifyIORef' classesRef (+ isClasses s)
          writeIORef batchRef []

      processLine line = do
        modifyIORef' totalRef (+1)
        if null line
          then pure ()
          else case parseSR alg (B.pack varnames) False (B.pack line) of
            Left err -> do
              modifyIORef' failedRef (+1)
              unless ingestQuiet $ hPutStrLn stderr $ "  FAILED: " ++ line ++ " -- " ++ err
            Right tree -> do
              modifyIORef' validRef (+1)
              modifyIORef' batchRef ((relabelParams tree, [], Nothing) :)
              batch <- readIORef batchRef
              when (length batch >= batchSize) $ do
                flushBatch
                v <- readIORef validRef
                when (v `mod` progressInterval < batchSize) $ do
                  hPutStrLn stderr $ "  ... " ++ show v ++ " expressions processed"
                  hFlush stderr

      loop = do
        done <- hIsEOF h
        if done then pure ()
        else do
          line <- hGetLine h
          processLine line
          loop

  loop
  flushBatch  -- insert any remaining expressions

  -- Close file handle
  unless (null ingestExprs) (hClose h)

  -- Summary
  total  <- readIORef totalRef
  valid  <- readIORef validRef
  failed <- readIORef failedRef
  classes <- readIORef classesRef
  putStrLn $ "Parsed " ++ show total ++ " expressions ("
           ++ show valid ++ " valid, " ++ show failed ++ " failed)"
  putStrLn $ "Imported into " ++ ingestDb
           ++ maybe "" (\d -> " [dataset: " ++ d ++ "]") mds
           ++ ": " ++ show classes ++ " e-classes"

  close db

  when (ingestEqsatSteps > 0) $
    putStrLn "(eqsat after ingest not yet implemented in standalone CLI)"
