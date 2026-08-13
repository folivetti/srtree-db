{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.HUnit
import Control.Monad.Identity (runIdentity, Identity)
import Control.Monad.State.Strict (runStateT, execStateT, evalStateT)
import qualified Data.IntMap as IntMap
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as VU
import Data.Text (Text)
import Data.SRTree.Eval (Target)

import Database.SQLite3 (Database, SQLData(..), exec, close, open)

import Data.SRTree
import Algorithm.EqSat
import Algorithm.EqSat.Egraph
import Algorithm.EqSat.Build (fromTree)
import Algorithm.EqSat.Info (insertFitness)
import Algorithm.EqSat.DB (Pattern(..), match)
import Algorithm.EqSat.Storage.SQLite
import Algorithm.EqSat.Storage.Query

myCost :: SRTree Int -> Int
myCost (Var _)     = 1
myCost (Const _)   = 1
myCost (Param _)   = 1
myCost (Bin _ l r) = 2 + l + r
myCost (Uni _ t)   = 3 + t

runIn :: EGraph -> EGraphST Identity a -> (a, EGraph)
runIn g m = runIdentity $ runStateT m g

evalIn :: EGraph -> EGraphST Identity a -> a
evalIn g m = runIdentity $ evalStateT m g

-- | x0..x3; add = x0+x1 (fit 0.9), p2 = (x0+x1)*x2 (fit 0.7),
--            p3 = (x0+x1)*x3 (fit 0.5). Returns (eg, add, p2, p3).
buildGraph :: IO (EGraph, EClassId, EClassId, EClassId)
buildGraph = pure (eg, eidAdd, eidP2, eidP3)
  where
    ((eidAdd, eidP2, eidP3), eg) = runIn emptyGraph go
    go = do
      _      <- fromTree myCost (var 0)
      _      <- fromTree myCost (var 1)
      eidAdd <- fromTree myCost (var 0 + var 1)
      eidP2  <- fromTree myCost ((var 0 + var 1) * var 2)
      eidP3  <- fromTree myCost ((var 0 + var 1) * var 3)
      insertFitness eidAdd 0.9 []
      insertFitness eidP2 0.7 []
      insertFitness eidP3 0.5 [VU.fromList [1.0, 2.0]]
      pure (eidAdd, eidP2, eidP3)

numEvaluated :: EGraph -> Int
numEvaluated eg = length
  [ () | (_, ec) <- IntMap.toList (_eClass eg), _fitness (_info ec) /= Nothing ]

-- ---------------------------------------------------------------------------

test_saveLoadRT :: Test
test_saveLoadRT = TestCase $ do
  db <- open ":memory:"
  (eg, eidAdd, _, eidP3) <- buildGraph
  _ <- saveGraph db eg
  m <- loadGraph db
  case m of
    Left err -> assertFailure ("loadGraph failed: " <> err)
    Right eg' -> do
      assertEqual "class count preserved" (IntMap.size (_eClass eg)) (IntMap.size (_eClass eg'))
      assertEqual "fitness preserved" (Just 0.9) (evalIn eg' (getFitness eidAdd))
      -- theta round-trips on the class that had params
      let th = evalIn eg' (getTheta eidP3)
      assertEqual "theta preserved" 1 (length th)
      -- (x0+x1)*(x2|x3) must still be matchable after import
      let prod = Fixed (Bin Mul (Fixed (Bin Add (VarPat 'A') (VarPat 'B'))) (VarPat 'C'))
      assertBool "patterns queryable after load"
        (length (evalIn eg' (match prod)) >= 2)
      -- inserting the already-loaded (x0+x1) dedups (no new class)
      let (eid2, g2) = runIn eg' (fromTree myCost (var 0 + var 1))
      assertEqual "post-load dedup" eidAdd eid2
      assertEqual "post-load no growth" (IntMap.size (_eClass eg)) (IntMap.size (_eClass g2))
  close db

-- | Queries over the stored fit table.
test_queries :: Test
test_queries = TestCase $ do
  db <- open ":memory:"
  (eg, eidAdd, eidP2, _) <- buildGraph
  _ <- saveGraph db eg
  tn2 <- topN db 2
  case tn2 of
    (x : _) -> assertEqual "topN[0]" (eidAdd, 0.9) x
    []      -> assertFailure "topN returned no rows"
  assertEqual "topN length" 2 (length tn2)
  tn1 <- topN db 1
  assertEqual "topN(1)" [(eidAdd, 0.9)] tn1
  dc <- distributionCounts db 100
  assertEqual "distribution totals evaluated classes" (numEvaluated eg) (sum (map snd dc))
  cAdd <- countPattern db "EAdd"
  assertEqual "EAdd classes" 1 cAdd
  cMul <- countPattern db "EMul"
  assertEqual "EMul classes" 2 cMul
  -- pareto over (max fitness, min dl): give add (fit 0.9, dl 1.0) which
  -- dominates p2 (0.7, 2.0) and p3 (0.5, 3.0), so only add remains.
  exec db ("UPDATE fit SET dl = 1.0 WHERE eid = " <> T.pack (show eidAdd))
  exec db ("UPDATE fit SET dl = 2.0 WHERE eid = " <> T.pack (show eidP2))
  exec db ("UPDATE fit SET dl = 3.0 WHERE eid NOT IN (" <> T.pack (show eidAdd) <> "," <> T.pack (show eidP2) <> ")")
  p <- pareto db
  assertEqual "pareto keeps only add" [(eidAdd, 0.9, 1.0)] p
  close db

-- | pushFit / refreshFitness sync risk metrics between graph and DB.
test_sync :: Test
test_sync = TestCase $ do
  db <- open ":memory:"
  (eg, eidAdd, _, _) <- buildGraph
  _ <- saveGraph db eg

  -- refit in memory, push to DB
  let egNew = runIdentity $ execStateT (insertFitness eidAdd 0.99 []) eg
  pushFit db egNew
  Right eg3 <- refreshFitness db eg
  assertEqual "pushed fitness read back" (Just 0.99) (evalIn eg3 (getFitness eidAdd))
  assertEqual "graph class count intact" (IntMap.size (_eClass eg)) (IntMap.size (_eClass eg3))

  -- edit fitness straight in the DB, refresh pulls it in
  exec db ("UPDATE fit SET fitness = 0.55 WHERE eid = " <> T.pack (show eidAdd))
  Right eg4 <- refreshFitness db eg
  assertEqual "db edit pulled in" (Just 0.55) (evalIn eg4 (getFitness eidAdd))

  -- a stored graph loads with the current DB fitness
  Right eg5 <- loadGraph db
  assertEqual "load uses current DB fitness" (Just 0.55) (evalIn eg5 (getFitness eidAdd))
  close db

main :: IO ()
main = do
  counts <- runTestTT $ TestList
    [ TestLabel "save-load-roundtrip" test_saveLoadRT
    , TestLabel "queries"             test_queries
    , TestLabel "sync"                test_sync
    ]
  if failures counts /= 0 || errors counts /= 0
    then error "Some tests failed"
    else pure ()