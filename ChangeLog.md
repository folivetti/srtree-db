# Changelog for srtree-db

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
