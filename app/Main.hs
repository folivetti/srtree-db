{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative
import Ingest (IngestOpts, ingestParser, runIngest)
import EqSat  (EqSatOpts, eqsatParser, runEqSatCmd)
import FitData (FitDataOpts, fitdataParser, runFitData, runRefit)
import Status (StatusOpts, statusParser, runStatus)
import Export (ExportOpts, exportParser, runExport)
import Backfill (BackfillOpts, backfillParser, runBackfill)
import RandomSampler (RandomSamplerOpts, randomSamplerParser, runRandomSampler)

data Cmd = Ingest IngestOpts | EqSat EqSatOpts | FitData FitDataOpts | Refit FitDataOpts | Status StatusOpts | ExportCmd ExportOpts | BackfillCmd BackfillOpts | RandomSamplerCmd RandomSamplerOpts

main :: IO ()
main = execParser cmdParser >>= dispatch

cmdParser :: ParserInfo Cmd
cmdParser = info (subcommands <**> helper) (progDesc "srtree-db: e-graph database CLI")
  where
    subcommands = subparser
      (  command "ingest"  (Ingest  <$> info (ingestParser <**> helper) (progDesc "Ingest expressions into DB"))
      <> command "eqsat"   (EqSat   <$> info (eqsatParser <**> helper) (progDesc "Run equality saturation"))
      <> command "fitdata" (FitData <$> info (fitdataParser <**> helper) (progDesc "Fit expressions to dataset"))
      <> command "refit"   (Refit   <$> info (fitdataParser <**> helper) (progDesc "Clear fit data and re-fit all expressions"))
      <> command "status"  (Status  <$> info (statusParser <**> helper) (progDesc "Show fit status for a dataset"))
      <> command "export"  (ExportCmd <$> info (exportParser <**> helper) (progDesc "Export fitted expressions as CSV"))
      <> command "backfill-parents" (BackfillCmd <$> info (backfillParser <**> helper) (progDesc "Backfill enode_parent reverse index for existing DBs"))
      <> command "random-sampler" (RandomSamplerCmd <$> info (randomSamplerParser <**> helper) (progDesc "Sample N random fitted expressions and print sorted by fitness"))
      )

dispatch :: Cmd -> IO ()
dispatch (Ingest  opts) = runIngest opts
dispatch (EqSat   opts) = runEqSatCmd opts
dispatch (FitData opts) = runFitData opts
dispatch (Refit   opts) = runRefit opts
dispatch (Status  opts) = runStatus opts
dispatch (ExportCmd opts) = runExport opts
dispatch (BackfillCmd opts) = runBackfill opts
dispatch (RandomSamplerCmd opts) = runRandomSampler opts
