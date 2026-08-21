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

import Data.SRTree (SRTree(..))
import Algorithm.EqSat (runEqSat)
import Algorithm.EqSat.Egraph (EGraph(..), EClassPageStore(..))
import Algorithm.EqSat.Simplify (rewrites, rewritesParams, myCost)
import Algorithm.EqSat.Storage.Backend (SqlBackend)
import Algorithm.EqSat.Storage.SQLite (loadGraphLazy, saveGraph, flushStore)
import Algorithm.EqSat.Storage.Query (getOrCreateDataset)

import Database.SQLite3 (Database, open, close)

-- | CLI options for the eqsat sub-command.
data EqSatOpts = EqSatOpts
  { eqsatDb      :: String
  , eqsatDataset :: String
  , eqsatSteps   :: Int
  , eqsatRuleset :: String
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

-- | Run the eqsat sub-command.
runEqSatCmd :: EqSatOpts -> IO ()
runEqSatCmd EqSatOpts{..} = do
  let rules = case eqsatRuleset of
                "params" -> rewritesParams
                _        -> rewrites

  putStrLn $ "Loading paged graph from " ++ eqsatDb ++ "..."
  r <- withSQLite eqsatDb $ \db -> do
    dsid <- getOrCreateDataset db eqsatDataset
    er <- loadGraphLazy db dsid
    case er of
      Left err -> pure (Left err)
      Right eg -> do
        let classCount = IntMap.size (_eClass eg)
        putStrLn $ "Loaded " ++ show classCount ++ " e-classes"
        putStrLn $ "Running " ++ show eqsatSteps ++ " steps of eqsat with '"
                 ++ eqsatRuleset ++ "' rules..."
        let go g = execStateT (runEqSat myCost rules eqsatSteps) g
        eg' <- go eg
        let classCount' = IntMap.size (_eClass eg')
        flushStore eg'
        saveResult <- saveGraph db dsid eg'
        case saveResult of
          Left err -> pure (Left ("saveGraph failed: " ++ err))
          Right _  -> do
            -- clear frontier after full eqsat
            case _classStore eg' of
              Nothing -> pure ()
              Just h  -> cpsEndFrontier h
            pure (Right (classCount, classCount'))

  case r of
    Left err -> putStrLn $ "eqsat failed: " ++ err
    Right (before, after) -> do
      putStrLn $ "After eqsat: " ++ show after ++ " e-classes ("
               ++ show (after - before) ++ " change from " ++ show before ++ ")"
      putStrLn $ "Saved to " ++ eqsatDb ++ " [dataset: " ++ eqsatDataset ++ "]"

-- | Open a SQLite database, run an action, and close it.
withSQLite :: String -> (Database -> IO a) -> IO a
withSQLite path = bracket (open (T.pack path)) close
