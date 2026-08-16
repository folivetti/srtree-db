{-# LANGUAGE OverloadedStrings #-}

-- | Persistent representation helpers: canonical e-node content keys and
-- theta serialization used by the SQL storage layer.
module Algorithm.EqSat.Storage.Types
  ( enodeKey
  , parseEnodeKey
  , enodeChildren
  , enodeOpTag
  , enodeOpDetail
  , opDetailOf
  , serializeTheta
  , parseTheta
  ) where

import Data.SRTree.Internal (Op(..), Function(..), SRTree(..))
import Data.SRTree.Eval (Target)
import Algorithm.EqSat.Egraph (ENode(..), EClassId, NOp(..))

import qualified Data.IntMap as IntMap
import qualified Data.Vector.Unboxed as VU
import Data.List (intercalate)
import Data.Char (isSpace)
import Text.Read (readMaybe)

-- | Canonical, stable, parseable serialization of an e-node.
--
-- Children reference e-class ids: for EUni/EBin they appear inline, for the
-- ENAry multiset they appear as @class:multiplicity@ pairs (sorted by class).
-- This string doubles as the content-addressable primary key of @enode@.
enodeKey :: ENode -> String
enodeKey (EVar i)    = "Var " <> show i
enodeKey (EParam i)  = "Param " <> show i
enodeKey (EConst x)  = "Const " <> show x
enodeKey (EUni f c)  = "Uni " <> show f <> " " <> show c
enodeKey (EBin op l r) = "Bin " <> show op <> " " <> show l <> " " <> show r
enodeKey (ENAry op m) = "NAry " <> showNOp op <> " " <> intercalate " " [show c <> ":" <> show n | (c, n) <- IntMap.toAscList m]

showNOp :: NOp -> String
showNOp EAdd = "EAdd"
showNOp EMul = "EMul"

parseEnodeKey :: String -> Maybe ENode
parseEnodeKey = go . words
  where
    go ["Var", i]          = EVar <$> readMaybe i
    go ["Param", i]        = EParam <$> readMaybe i
    go ["Const", x]        = EConst <$> readMaybe x
    go ["Uni", f, c]       = do
      fc <- readMaybe f
      cc <- readMaybe c
      pure (EUni fc cc)
    go ["Bin", op, l, r]   = do
      oc <- readMaybe op
      lc <- readMaybe l
      rc <- readMaybe r
      pure (EBin oc lc rc)
    go ("NAry" : op : rest) = do
      oc <- parseNOp op
      ch <- mapM parseChild rest
      pure (ENAry oc (IntMap.fromList ch))
    go _ = Nothing

    parseChild w = case break (== ':') w of
      (c, ':' : n) -> (,) <$> readMaybe c <*> readMaybe n
      _            -> Nothing

parseNOp :: String -> Maybe NOp
parseNOp "EAdd" = Just EAdd
parseNOp "EMul" = Just EMul
parseNOp _      = Nothing

-- | All e-class ids referenced as children by a node.
enodeChildren :: ENode -> [EClassId]
enodeChildren (EUni _ c)   = [c]
enodeChildren (EBin _ l r) = [l, r]
enodeChildren (ENAry _ m)  = IntMap.keys m
enodeChildren _            = []

-- | Coarse operator tag used as the @op@ column: Var | Param | Const | Uni | Bin | NAry.
enodeOpTag :: ENode -> String
enodeOpTag (EVar _)    = "Var"
enodeOpTag (EParam _)  = "Param"
enodeOpTag (EConst _)  = "Const"
enodeOpTag (EUni _ _)  = "Uni"
enodeOpTag (EBin _ _ _) = "Bin"
enodeOpTag (ENAry _ _) = "NAry"

-- | Specific operator detail (EAdd/EMul/Add/Sub/.../LogAbs/...) for pattern counting.
enodeOpDetail :: ENode -> String
enodeOpDetail (EVar _)    = "Var"
enodeOpDetail (EParam _)  = "Param"
enodeOpDetail (EConst _)  = "Const"
enodeOpDetail (EUni f _)  = show f
enodeOpDetail (EBin op _ _) = show op
enodeOpDetail (ENAry op _)  = showNOp op

-- | The @op_detail@ column value matching an operator-key shape (@SRTree ()@,
-- as produced by 'Algorithm.EqSat.Egraph.eOpKey' / pattern @opOf@). N-ary
-- Add/Mul patterns address the flattened ENAry nodes, whose @op_detail@ is
-- @EAdd@/@EMul@, so the binary shapes @Bin Add@/@Bin Mul@ map there; other
-- operators map to their own detail.
opDetailOf :: SRTree () -> String
opDetailOf (Var _)    = "Var"
opDetailOf (Param _)  = "Param"
opDetailOf (Const _)  = "Const"
opDetailOf (Y _)      = "Var"
opDetailOf (Uni f _)  = show f
opDetailOf (Bin Add _ _) = showNOp EAdd
opDetailOf (Bin Mul _ _) = showNOp EMul
opDetailOf (Bin op _ _)  = show op

-- | Flatten a list of target vectors: vectors joined by @|@, elements by @,@.
serializeTheta :: [Target] -> String
serializeTheta ts = intercalate "|" [ intercalate "," (map show (VU.toList t)) | t <- ts ]

-- | Inverse of 'serializeTheta'.
parseTheta :: String -> [Target]
parseTheta s = [ VU.fromList (map readDouble (splitOn ',' v)) | v <- splitOn '|' s, not (null (filter (not . isSpace) v)) ]
  where
    readDouble x = case readMaybe x of
      Just d  -> d
      Nothing -> 0

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (a, []   ) -> if null a then [] else [a]
  (a, _ : b) -> a : splitOn c b