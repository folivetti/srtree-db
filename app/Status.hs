{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Status
  ( StatusOpts(..)
  , statusParser
  , runStatus
  ) where

import qualified Data.Text as T
import Options.Applicative
import System.IO (hPutStrLn, hFlush, stdout, stderr)

import Algorithm.EqSat.Storage.Backend (SqlBackend(..), SqlValue(..), sqlToInt)
import Algorithm.EqSat.Storage.Schema (createSchemaFit)
import Algorithm.EqSat.Storage.SQLite ()
import Database.SQLite3 (Database, open, close, exec)
import Control.Exception (bracket)

data StatusOpts = StatusOpts
  { statusEgraph    :: String
  , statusFitdb     :: String
  , statusDataset   :: String
  } deriving (Show)

statusParser :: Parser StatusOpts
statusParser = StatusOpts
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

runStatus :: StatusOpts -> IO ()
runStatus StatusOpts{..} = do
  withSQLite statusFitdb $ \fitDb -> do
    createSchemaFit fitDb
    -- Look up dataset id (don't create if missing)
    dsRows <- queryDb fitDb "SELECT id FROM dataset WHERE name = ?"
      [SqlText (T.pack statusDataset)]
    case dsRows of
      [] -> do
        putStrLn $ "Dataset '" ++ statusDataset ++ "' not found."
        hFlush stdout
        pure ()
      [[dsIdVal]] -> do
        let dsid = sqlToInt dsIdVal
        putStrLn $ "Dataset: " ++ statusDataset ++ " (id=" ++ show dsid ++ ")"
        hFlush stdout

        withSQLite statusEgraph $ \egDb -> do
          totalRows <- queryDb egDb "SELECT COUNT(*) FROM eclass" []
          let totalEclasses = case totalRows of { [[cnt]] -> sqlToInt cnt; _ -> 0 }

          fittedRows <- queryDb fitDb
            "SELECT COUNT(*) FROM dataset_fit WHERE dataset_id = ? AND fitted = 1"
            [SqlInteger (fromIntegral dsid)]
          let totalFitted = case fittedRows of { [[cnt]] -> sqlToInt cnt; _ -> 0 }

          finiteRows <- queryDb fitDb
            "SELECT COUNT(*) FROM dataset_fit WHERE dataset_id = ? AND fitted = 1 AND fitness IS NOT NULL"
            [SqlInteger (fromIntegral dsid)]
          let totalFinite = case finiteRows of { [[cnt]] -> sqlToInt cnt; _ -> 0 }

          prunedRows <- queryDb fitDb
            "SELECT COUNT(*) FROM dataset_fit WHERE dataset_id = ? AND fitted = 1 AND fitness IS NULL"
            [SqlInteger (fromIntegral dsid)]
          let totalPruned = case prunedRows of { [[cnt]] -> sqlToInt cnt; _ -> 0 }

          let totalUnfitted = totalEclasses - totalFitted

          putStrLn $ "  Total eclasses:  " ++ show totalEclasses
          putStrLn $ "  Fitted:          " ++ show totalFitted
          putStrLn $ "    Finite fitness:  " ++ show totalFinite
          putStrLn $ "    Pruned (NULL):   " ++ show totalPruned
          putStrLn $ "  Unfitted:        " ++ show totalUnfitted
          hFlush stdout
      _ -> do
        putStrLn $ "Dataset '" ++ statusDataset ++ "' query returned unexpected result."
        hFlush stdout

withSQLite :: String -> (Database -> IO a) -> IO a
withSQLite path f = bracket openDb close f
  where
    openDb = do
      db <- open (T.pack path)
      exec db "PRAGMA busy_timeout = 5000"
      pure db
