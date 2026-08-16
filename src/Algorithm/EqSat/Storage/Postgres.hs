{-# LANGUAGE OverloadedStrings #-}

-- | PostgreSQL-backed persistence for srtree e-graphs.
--
-- Implements the same driver-neutral interface as
-- 'Algorithm.EqSat.Storage.SQLite' on top of @libpq@
-- ('Database.PostgreSQL.LibPQ'), so the shared storage code
-- ('saveGraph'/'loadGraph'/'pushFit'/'refreshFitness' and the
-- 'Algorithm.EqSat.Storage.Query' API) runs unchanged against PostgreSQL.
--
-- Connections are plain @libpq@ connections (see 'connectPostgres' /
-- 'closePostgres'); the reggression layer dispatches on a @postgres://@ /
-- @postgresql://@ DSN.
module Algorithm.EqSat.Storage.Postgres
  ( schemaPostgres
  , connectPostgres
  , closePostgres
  ) where

import Control.Monad (forM, forM_)
import Data.ByteString (ByteString)
import qualified Data.IntSet as IntSet
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.LibPQ
  ( Connection, ExecStatus(..), Format(..), Oid(..), Result
  , connectdb, exec, execParams, finish, getvalue, invalidOid, nfields
  , ntuples, resultErrorMessage, resultStatus, toColumn, toRow )

import Algorithm.EqSat.Storage.Backend (SqlValue(..), SqlBackend(..), sqlToInt, sqlToText)
import Algorithm.EqSat.Storage.ClassStore (unhex)

-- | PostgreSQL DDL. Mirrors 'Algorithm.EqSat.Storage.Schema.schemaSQL'.
--
-- Differences from SQLite: @BIGINT@ identity keys, @DOUBLE PRECISION@
-- metrics, and foreign keys declared @DEFERRABLE INITIALLY DEFERRED@ so the
-- writer can insert @eclass_node@/@fit@ rows before their referenced
-- @eclass@ rows within the @BEGIN@..@COMMIT@ transaction of 'saveGraph'.
schemaPostgres :: [Text]
schemaPostgres =
  [ "CREATE TABLE IF NOT EXISTS meta ("
    <> " key TEXT PRIMARY KEY,"
    <> " value TEXT NOT NULL)"
  , "CREATE TABLE IF NOT EXISTS enode ("
    <> " key TEXT PRIMARY KEY,"
    <> " op TEXT NOT NULL,"
    <> " op_detail TEXT,"
    <> " a BIGINT,"
    <> " b BIGINT,"
    <> " x DOUBLE PRECISION)"
  , "CREATE TABLE IF NOT EXISTS enode_child ("
    <> " enode_key TEXT NOT NULL REFERENCES enode(key) ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED,"
    <> " child_eid BIGINT NOT NULL,"
    <> " cnt INTEGER NOT NULL DEFAULT 1,"
    <> " PRIMARY KEY (enode_key, child_eid))"
  , "CREATE TABLE IF NOT EXISTS eclass ("
    <> " eid BIGINT PRIMARY KEY,"
    <> " canonical BIGINT NOT NULL,"
    <> " height INTEGER NOT NULL DEFAULT 0)"
  , "CREATE TABLE IF NOT EXISTS eclass_node ("
    <> " eid BIGINT NOT NULL REFERENCES eclass(eid) ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED,"
    <> " enode_key TEXT NOT NULL REFERENCES enode(key) ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED,"
    <> " PRIMARY KEY (eid, enode_key))"
  , "CREATE TABLE IF NOT EXISTS parent ("
    <> " child_eid BIGINT NOT NULL REFERENCES eclass(eid) ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED,"
    <> " parent_eid BIGINT NOT NULL,"
    <> " parent_enode_key TEXT NOT NULL REFERENCES enode(key) ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED,"
    <> " PRIMARY KEY (child_eid, parent_eid, parent_enode_key))"
  , "CREATE TABLE IF NOT EXISTS fit ("
    <> " eid BIGINT PRIMARY KEY REFERENCES eclass(eid) ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED,"
    <> " fitness DOUBLE PRECISION,"
    <> " dl DOUBLE PRECISION,"
    <> " theta TEXT,"
    <> " size INTEGER NOT NULL DEFAULT 0)"
  , "CREATE INDEX IF NOT EXISTS idx_fit_fitness ON fit(fitness)"
  , "CREATE INDEX IF NOT EXISTS idx_fit_size ON fit(size)"
  , "CREATE INDEX IF NOT EXISTS idx_fit_dl ON fit(dl)"
  , "CREATE TABLE IF NOT EXISTS cstore_page ("
    <> " key TEXT PRIMARY KEY,"
    <> " blob TEXT NOT NULL)"
  , "CREATE TABLE IF NOT EXISTS frontier ("
    <> " eid BIGINT PRIMARY KEY REFERENCES eclass(eid) ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED,"
    <> " updated_at TEXT)"
  , "CREATE TABLE IF NOT EXISTS dataset ("
    <> " id BIGSERIAL PRIMARY KEY,"
    <> " name TEXT NOT NULL UNIQUE,"
    <> " created TEXT)"
  , "CREATE TABLE IF NOT EXISTS dataset_fit ("
    <> " dataset_id BIGINT NOT NULL REFERENCES dataset(id) ON DELETE CASCADE,"
    <> " eid BIGINT NOT NULL REFERENCES eclass(eid) ON DELETE CASCADE,"
    <> " fitness DOUBLE PRECISION,"
    <> " dl DOUBLE PRECISION,"
    <> " theta TEXT,"
    <> " size INTEGER NOT NULL DEFAULT 0,"
    <> " evaluated INTEGER NOT NULL DEFAULT 0,"
    <> " fitted INTEGER NOT NULL DEFAULT 0,"
    <> " stale INTEGER NOT NULL DEFAULT 0,"
    <> " updated_at TEXT,"
    <> " PRIMARY KEY (dataset_id, eid))"
  , "CREATE INDEX IF NOT EXISTS idx_dsfit_fitness ON dataset_fit(fitness)"
  , "CREATE INDEX IF NOT EXISTS idx_dsfit_size ON dataset_fit(size)"
  , "CREATE INDEX IF NOT EXISTS idx_dsfit_dl ON dataset_fit(dl)"
  , "CREATE TABLE IF NOT EXISTS expression_index ("
    <> " expression_key TEXT PRIMARY KEY,"
    <> " eclass BIGINT NOT NULL REFERENCES eclass(eid) ON DELETE CASCADE,"
    <> " dataset_id BIGINT REFERENCES dataset(id) ON DELETE CASCADE,"
    <> " first_seen TEXT)"
  ]

-- | Open a PostgreSQL connection from a connection string (e.g.
-- @postgresql://user:pass@host:5432/db@).
connectPostgres :: String -> IO Connection
connectPostgres = connectdb . TE.encodeUtf8 . T.pack

-- | Close a PostgreSQL connection.
closePostgres :: Connection -> IO ()
closePostgres = finish

instance SqlBackend Connection where
  execDb conn sql = do
    r <- pgExec conn sql
    statusOK r "exec"

  runDb conn sql params = do
    r <- pgExecParams conn sql params
    statusOK r "run"

  insertIgnore conn tail params = do
    r <- pgExecParams conn ("INSERT INTO " <> tail <> " ON CONFLICT DO NOTHING") params
    statusOK r "insertIgnore"

  queryDb conn sql params = do
    r <- pgExecParams conn sql params
    st <- resultStatus r
    case st of
      TuplesOk -> do
        ns <- ntuples r
        nf <- nfields r
        let n = fromEnum ns
            m = fromEnum nf
        forM [0 .. n - 1] $ \i ->
          forM [0 .. m - 1] $ \j -> do
            v <- getvalue r (toRow i) (toColumn j)
            pure $ case v of
              Nothing -> SqlNull
              Just bs -> SqlText (TE.decodeUtf8 bs)
      _ -> do
        statusOK r "query"
        pure []

  createSchemaDb conn = mapM_ (execDb conn) schemaPostgres

  -- Grid fallback (Postgres is not the out-of-core target): the cursor-based
  -- streaming matcher needs 'Database.SQLite3'; here we return the full
  -- distinct set up to @budget@, documented as unbounded memory.
  streamByOp conn opDetail budget exclude = do
    let ex = IntSet.fromList exclude
    rows <- queryDb conn
      "SELECT DISTINCT n.eid FROM eclass_node n \
      \JOIN enode e ON e.key = n.enode_key WHERE e.op_detail = ?"
      [SqlText opDetail]
    pure (take budget [ eid | [eid'] <- rows, let eid = sqlToInt eid', not (IntSet.member eid ex) ])
  -- Grid fallback for page streaming (unbounded; Postgres is not the
  -- out-of-core target).
  streamPages conn tbl k = do
    rows <- queryDb conn ("SELECT key, blob FROM " <> tbl) []
    forM_ rows $ \[key, blob] -> k (fromIntegral (sqlToInt key)) (unhex (sqlToText blob))

-- | Raise an exception unless the status is @CommandOk@/@TuplesOk@.
statusOK :: Result -> Text -> IO ()
statusOK r tag = do
  st <- resultStatus r
  case st of
    CommandOk  -> pure ()
    TuplesOk   -> pure ()
    EmptyQuery -> pure ()
    _          -> do
      mmsg <- resultErrorMessage r
      let msg = maybe "unknown error" (T.unpack . TE.decodeUtf8) mmsg
      fail ("postgres: " <> T.unpack tag <> ": " <> msg)

-- | Execute a statement without parameters (DDL, BEGIN/COMMIT, DELETE).
pgExec :: Connection -> Text -> IO Result
pgExec conn sql = do
  mr <- exec conn (TE.encodeUtf8 sql)
  case mr of
    Nothing -> fail "postgres: exec returned no result"
    Just r  -> pure r

-- | Execute a parameterized statement, rewriting @?@ to @$n@.
pgExecParams :: Connection -> Text -> [SqlValue] -> IO Result
pgExecParams conn sql params = do
  let pgSql = toPG sql
      ps    = map renderParam params
  mr <- execParams conn (TE.encodeUtf8 pgSql) ps Text
  case mr of
    Nothing -> fail "postgres: execParams returned no result"
    Just r  -> pure r

-- | Render a parameter for libpq's text-format protocol. NULL is never sent
-- as a parameter: the shared SQL spells it as the literal @NULL@.
renderParam :: SqlValue -> Maybe (Oid, ByteString, Format)
renderParam (SqlInteger n) = Just (invalidOid, TE.encodeUtf8 (T.pack (show n)), Text)
renderParam (SqlFloat d)   = Just (invalidOid, TE.encodeUtf8 (T.pack (show d)), Text)
renderParam (SqlText t)    = Just (invalidOid, TE.encodeUtf8 t, Text)
renderParam SqlNull        = Nothing

-- | Rewrite the shared positional @?@ placeholders to libpq's @$n@ form
-- (single-quoted literals are skipped, in case a value embeds @?@).
toPG :: Text -> Text
toPG = T.pack . go (1 :: Int) . T.unpack
  where
    go :: Int -> String -> String
    go _ []          = []
    go n ('\'' : r)  = '\'' : skip r
      where
        skip ('\'' : r') = '\'' : go n r'
        skip (c : r')    = c : skip r'
        skip []          = []
    go n ('?' : r)    = '$' : show n ++ go (n + 1) r
    go n (c : r)      = c : go n r