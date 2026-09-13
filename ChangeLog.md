# Changelog for srtree-db

## 0.1.3.1

- **Fix**: spawning multiple threads even when using `-N` flag to limit it. 

## 0.1.3.0

- **New CLI subcommands**:
  - `export`: export fitted expressions as CSV (`expression,length,fitness`). Supports `--finite` flag to exclude NaN/invalid expressions.
  - `backfill-parents`: backfill `enode_parent` reverse index for existing DBs.
  - `random-sampler`: sample N random fitted expressions and print sorted by fitness.
- **Split-DB fixes**:
  - `DBInsert`: create egraph schema before first insert (fixes 'no such table: eclass_node' error).
  - `recordExpressionIndex`: correctly handle separate egraph and fit DB files.
  - `loadGraphLazy`: treat missing meta table as 'no e-graph stored' instead of error.
  - `emptyPagedGraph`: new function to seed a fresh DB on first out-of-core insert.
  - Export `createSchemaFit` for the DB-native import path.
- **Concurrency fixes**: fixed concurrency problems in fitting with separate DB structures.
- **Memory fixes**: `top` command no longer uses all memory on large datasets.

## 0.1.2.0

- **Split-DB architecture**: e-graph and per-dataset fit data now live in separate SQLite files to eliminate WAL bloat during fitting. Schema split into `egraphSchemaSQL` and `fitSchemaSQL`.
- **Blob format**: switched `cstore_page` from hex-encoded TEXT to binary BLOB; added `SqlBlob` and `loadPagesBulk` for bulk page preloading.
- **New CLI subcommands**: `ingest` (streaming line-by-line expression parser with batched inserts), `eqsat` (standalone eqsat on paged graphs), `fitdata` (resumable batch fitting with periodic commit checkpoints), `status` (fit summary per dataset), `refit` (clear and re-fit from scratch).
- **fitdata performance overhaul**: batch page preloading (`loadPagesBulk`), persistent page cache, sub-expression expansion for bulk dependency loading, analytical fitting for parameter-free expressions, parallel fitting with `setMTPopParallel`, `--no-header` flag (default True), `compileLossAndGrad` reuse across restarts.
- **Ingest performance**: `writeNode` writes class pages inline (O(1) per new class); removed `writeMissingPages`. Added `--reparam` flag for float constants to parameters.
- **Frontier re-saturation**: `frontier` table marks changed classes; matcher restricted to frontier via `cpsBeginFrontier`/`cpsEndFrontier`.
- **Page streaming**: `pushFit` streams the page store via `SqlBackend.streamPages` (SQLite cursor / Postgres grid fallback), bounded O(1) memory.
- **Legacy cleanup**: removed `fit` table; `enode_child` populated during eqsat by write-through.
- **NaN propagation**: parameter-less subexpressions that evaluate to NaN/Infinity detected analytically and propagated to ancestors.
- **DB bloat fixes**: `INSERT OR REPLACE` replaced with upsert; batched transactions; secondary indexes dropped; `PRAGMA journal_mode=DELETE` in fitdata.
- **In-memory vs DB eqsat equivalence test**: `testEquivInMemDB` proves paged and in-memory eqsat converge to the same merge structure.
- **Extract module**: `extractTreeFromDB` (standalone SRTree reconstruction from DB pages) and `reconstructFromCache` (pure IntMap-based).
- **Conversion script**: `tools/convert_db.py` converts single-DB to split format.

## 0.1.1.0

- Added cli tools to populate and fit data into a database 

## 0.1.0.0

- Initial release
- Two-section data model: dataset-agnostic e-graph section + per-dataset fit section
- SQLite and PostgreSQL backends
- Out-of-core import with page store
- Dataset-aware queries: topN, pareto, paretoBySize, distributionCounts
- Frontier re-saturation support
- Streaming page export via cursor (O(1) memory)
