{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RandomSampler
  ( RandomSamplerOpts(..)
  , randomSamplerParser
  , runRandomSampler
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
import Control.Monad (forM_, when)
import Data.List (sortBy)
import Data.Ord (Down(..), comparing)

data RandomSamplerOpts = RandomSamplerOpts
  { rsEgraph  :: String
  , rsFitdb   :: String
  , rsDataset :: String
  , rsN       :: Int
  , rsFinite  :: Bool
  } deriving (Show)

randomSamplerParser :: Parser RandomSamplerOpts
randomSamplerParser = RandomSamplerOpts
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
  <*> option auto
      ( long "n"
      <> short 'n'
      <> metavar "INT"
      <> help "Number of expressions to sample" )
  <*> switch
      ( long "finite"
      <> short 'f'
      <> help "Only sample expressions with finite (non-NaN) fitness" )

runRandomSampler :: RandomSamplerOpts -> IO ()
runRandomSampler RandomSamplerOpts{..} = do
  when (rsN <= 0) $ do
    hPutStrLn stderr "Error: --n must be a positive integer."
    hFlush stderr
    fail "--n must be positive"

  withSQLite rsFitdb $ \fitDb -> do
    createSchemaFit fitDb
    dsRows <- queryDb fitDb "SELECT id FROM dataset WHERE name = ?"
      [SqlText (T.pack rsDataset)]
    case dsRows of
      [] -> do
        hPutStrLn stderr $ "Dataset '" ++ rsDataset ++ "' not found."
        hFlush stderr
      [[dsIdVal]] -> do
        let dsid = sqlToInt dsIdVal
            fitQuery
              | rsFinite =
                  "SELECT eid, fitness FROM dataset_fit \
                  \WHERE dataset_id = ? AND fitness IS NOT NULL \
                  \AND fitness * 0 = 0 \
                  \ORDER BY RANDOM() LIMIT ?"
              | otherwise =
                  "SELECT eid, fitness FROM dataset_fit \
                  \WHERE dataset_id = ? \
                  \ORDER BY RANDOM() LIMIT ?"
        rows <- queryDb fitDb fitQuery
          [SqlInteger (fromIntegral dsid), SqlInteger (fromIntegral rsN)]

        when (null rows) $ do
          hPutStrLn stderr $ "No fitted expressions found for dataset '" ++ rsDataset ++ "'."
          hFlush stderr

        let sampled = [ (sqlToInt eid, sqlToMaybeDouble fit)
                      | [eid, fit] <- rows
                      ]

        let sorted = sortBy (comparing (Down . snd)) sampled

        withSQLite rsEgraph $ \egDb -> do
          putStrLn "eid,fitness,expression"
          hFlush stdout
          forM_ sorted $ \(eid, mfit) -> do
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
                putStrLn $ show eid ++ "," ++ fitStr ++ "," ++ expr
                hFlush stdout

          let total = length sorted
              label = if rsFinite then " (finite)" else ""
          hPutStrLn stderr $ "Sampled " ++ show total ++ " expressions" ++ label ++ " from dataset '" ++ rsDataset ++ "'"
          hFlush stderr

      _ -> do
        hPutStrLn stderr $ "Dataset '" ++ rsDataset ++ "' query returned unexpected result."
        hFlush stderr

withSQLite :: String -> (Database -> IO a) -> IO a
withSQLite path f = bracket openDb close f
  where
    openDb = do
      db <- open (T.pack path)
      exec db "PRAGMA busy_timeout = 5000"
      pure db
