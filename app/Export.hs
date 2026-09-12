{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Export
  ( ExportOpts(..)
  , exportParser
  , runExport
  ) where

import qualified Data.Text as T
import Options.Applicative
import System.IO (hPutStrLn, hFlush, stdout, stderr)

import Data.SRTree.Print (showExpr)
import Algorithm.EqSat.Storage.Backend (SqlBackend(..), SqlValue(..), sqlToInt, sqlToMaybeDouble)
import Algorithm.EqSat.Storage.Extract (extractBestFromDB)
import Algorithm.EqSat.Storage.Schema (createSchemaFit)
import Algorithm.EqSat.Storage.SQLite ()
import Database.SQLite3 (Database, open, close, exec)
import Control.Exception (bracket)
import Control.Monad (forM_)

data ExportOpts = ExportOpts
  { exportEgraph  :: String
  , exportFitdb   :: String
  , exportDataset :: String
  , exportFinite  :: Bool
  } deriving (Show)

exportParser :: Parser ExportOpts
exportParser = ExportOpts
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
  <*> switch
      ( long "finite"
      <> short 'f'
      <> help "Only export expressions with finite (non-NaN) fitness" )

runExport :: ExportOpts -> IO ()
runExport ExportOpts{..} = do
  withSQLite exportFitdb $ \fitDb -> do
    createSchemaFit fitDb
    -- Look up dataset id (don't create if missing)
    dsRows <- queryDb fitDb "SELECT id FROM dataset WHERE name = ?"
      [SqlText (T.pack exportDataset)]
    case dsRows of
      [] -> do
        hPutStrLn stderr $ "Dataset '" ++ exportDataset ++ "' not found."
        hFlush stderr
      [[dsIdVal]] -> do
        let dsid = sqlToInt dsIdVal
        withSQLite exportEgraph $ \egDb -> do
          -- Query fit rows
          let fitQuery
                | exportFinite =
                    "SELECT eid, fitness, size FROM dataset_fit \
                    \WHERE dataset_id = ? AND fitness IS NOT NULL \
                    \ORDER BY eid"
                | otherwise =
                    "SELECT eid, fitness, size FROM dataset_fit \
                    \WHERE dataset_id = ? \
                    \ORDER BY eid"
          rows <- queryDb fitDb fitQuery [SqlInteger (fromIntegral dsid)]

          -- Print header
          putStrLn "expression,length,fitness"
          hFlush stdout

          -- Process each row
          forM_ rows (processRow egDb)

          let total = length rows
              label = if exportFinite then " (finite)" else ""
          hPutStrLn stderr $ "Exported " ++ show total ++ " expressions" ++ label ++ " from dataset '" ++ exportDataset ++ "'"
          hFlush stderr

      _ -> do
        hPutStrLn stderr $ "Dataset '" ++ exportDataset ++ "' query returned unexpected result."
        hFlush stderr

  where
    processRow egDb [eidVal, fitVal, szVal] = do
      let eid  = sqlToInt eidVal
          sz   = sqlToInt szVal
          mfit = sqlToMaybeDouble fitVal
      mTree <- extractBestFromDB egDb eid
      case mTree of
        Nothing -> do
          hPutStrLn stderr $ "WARNING: could not reconstruct expression for eid=" ++ show eid
          hFlush stderr
        Just tree -> do
          let expr = showExpr tree
              fitStr = case mfit of
                Nothing -> "NaN"
                Just f  -> show f
          putStrLn $ expr ++ "," ++ show sz ++ "," ++ fitStr
          hFlush stdout
    processRow _ _ = pure ()

withSQLite :: String -> (Database -> IO a) -> IO a
withSQLite path f = bracket openDb close f
  where
    openDb = do
      db <- open (T.pack path)
      exec db "PRAGMA busy_timeout = 5000"
      pure db
