# srtree-db

SQL persistence and out-of-core (paged) equality saturation for `srtree`
e-graphs. Driver-neutral over a tiny `SqlBackend` interface, so the same code runs
on SQLite and PostgreSQL.

## Architecture

Two layers:

- **Serialization / queries** (`Storage/`): `Schema.hs` defines a normalized
  schema (`meta`, `enode`, `enode_child`, `eclass`, `eclass_node`, `parent`,
  `dataset_fit`) plus a `cstore_page` key-value table. `SQLite.hs` / `Postgres.hs`
  provide the concrete `SqlBackend` instances and the public API
  (`saveGraph`, `loadGraph`, `loadGraphLazy`, `pushFit`, `refreshFitness`,
  `flushStore`). `Query.hs` is the SQL query API (`topN`, `pareto`,
  `paretoBySize`, `distributionCounts`, `countPattern`).
- **Paged e-class store** (`ClassStore.hs`): a bounded-LRU, write-back page store.
  Each e-class is a serialized `ByteString` page (hex-encoded `TEXT` so the same
  DDL runs on both drivers). An `EClassPageStore` handle adapts it to the
  `ClassStore` choke point in `Algorithm.EqSat.Egraph`.

## Two operating modes

The core e-graph is `ClassStore`-polymorphic, so the **same** eqsat code runs in
two modes:

| Mode | Resident map | Store | How to obtain |
|------|--------------|-------|---------------|
| In-memory | full | none (`Nothing`) | `emptyGraph`, `load`/`loadGraph` (eager) |
| Paged / out-of-core | empty (or bounded cache) | `Just handle` | `loadGraphLazy` |

`loadGraphLazy` leaves the resident e-class map **empty** and installs the page
store; only the structural indexes (canonical map, `node→class` hash map, pattern
trie, fit/size/DL range DBs) are resident. Equality saturation then streams every
e-class body through the store: `runEqSat` / `simplifyEqSat` /
`applyMergeOnlyEqSat` run in `IO` and never materialize the whole graph. New
e-nodes update the in-memory trie so later iterations see them.

The pure `Identity` / `State StdGen` instances keep the classic fully-resident
behavior. The `MonadIO` instance is the paged one: it consults the resident map
first, then the page store, and write-throughs on every mutation (batched
write-back at the flush threshold; call `flushStore` at commit points).

## Equality saturation directly on the database

Yes. `loadGraphLazy` → run `runEqSat` (or `eqSatStream`) in `IO` → `flushStore`
→ `saveGraph`. The pattern `testLazyRewrite` in the `srtree-db` test suite
exercises exactly this: a resident-empty graph is rewritten (including
store-aware `Condition`s such as `isNotZero`, which read class info from the
paged store) and the constant rewrites (`x-x→0`, `1**x→1`, `x/x→1`) land.

The `reggression` CLI exposes this as `db-eqsat <file> <iters> [ruleset]`.

## Caveats / known limitations

- **Whole-graph scans still pull everything.** Functions that call `allClasses`
  (`eqSat`'s final extraction, `recalculateBestAll`, `getEClassesThat`,
  `findRootClasses`) materialize every class at once in paged mode. Use the
  streaming variants `recalculateBestAllStream` / `recalculateBestStream` /
  `eqSatStream` to keep memory bounded. `runEqSat`'s main loop itself is
  streaming.
- **Structural indexes are resident.** `_canonicalMap`, `_eNodeToEClass` and the
  pattern trie grow in memory as rewriting adds nodes; only the *class-body*
  cache is bounded (`residentClassCap = 50000` + LRU `100000` pages).
- **Hybrid graphs must use IO monads.** A paged graph mutated through the pure
  `Identity` instance would diverge from the store; `saveGraph` now unions the
  resident map (authoritative for recent edits) over the persisted pages, so
  either mode's edits are preserved, but prefer IO (`runIOIn`) on paged graphs.
- **Durability:** writes are batched; call `flushStore` before relying on the DB.
- **Lazy trie pruning:** `seedEDB` seeds the pattern trie structurally but skips
  the constant-folding substitution the eager `rebuildDBs` does; matches remain
  genuine, just slightly less pruned.
