{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative
import Ingest (IngestOpts, ingestParser, runIngest)
import EqSat  (EqSatOpts, eqsatParser, runEqSatCmd)
import FitData (FitDataOpts, fitdataParser, runFitData, runRefit)

data Cmd = Ingest IngestOpts | EqSat EqSatOpts | FitData FitDataOpts | Refit FitDataOpts

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
      )

dispatch :: Cmd -> IO ()
dispatch (Ingest  opts) = runIngest opts
dispatch (EqSat   opts) = runEqSatCmd opts
dispatch (FitData opts) = runFitData opts
dispatch (Refit   opts) = runRefit opts
