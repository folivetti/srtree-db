#!/usr/bin/env bash
# export-workflow.sh — Generate expressions, ingest, eqsat, fit, and export
#
# Demonstrates the srtree-db split-DB workflow:
#   1. Generate 100 random expressions (some intentionally invalid)
#   2. Ingest into a combined DB
#   3. Convert to split-DB format (egraph.db + fit_demo.db)
#   4. Run equality saturation on the egraph
#   5. Fit expressions against a dataset
#   6. Export valid expressions as CSV
#
# Prerequisites:
#   cabal install srtree-db
#   python3 (for gen_expressions.py)
#
# Run from the tutorials/ directory:
#   bash export-workflow.sh

set -euo pipefail

SCRIPT_DIR="tutorials" # $(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SRTOOLS_DIR="" #"$(cd "$SCRIPT_DIR/.." && pwd)"
SRTREE_DB_DIR="" #"$SRTOOLS_DIR/srtree-db"
WORKDIR=$(mktemp -d)
trap 'rm -rf "$WORKDIR"' EXIT

# Helper: run srtree-db command from the correct directory
db_tool() {
  (srtree-db "$@")
}

echo "=== srtree-db export workflow demo ==="
echo "Working directory: $WORKDIR"
echo

# ---------------------------------------------------------------------------
# 1. Generate 100 random expressions (some invalid)
# ---------------------------------------------------------------------------
echo "--- Step 1: Generate 100 random expressions ---"
python3 "$SCRIPT_DIR/gen_expressions.py" 100 > "$WORKDIR/expressions.txt"
TOTAL_EXPR=$(wc -l < "$WORKDIR/expressions.txt")
echo "  Generated $TOTAL_EXPR expressions"
echo "  Sample (first 5):"
head -5 "$WORKDIR/expressions.txt" | sed 's/^/    /'
echo "    ..."
echo

# ---------------------------------------------------------------------------
# 2. Copy dataset
# ---------------------------------------------------------------------------
echo "--- Step 2: Prepare dataset ---"
cp "$SCRIPT_DIR/data.csv" "$WORKDIR/data.csv"
echo "  Dataset: data.csv (40 rows, y ~ f(x1, x2, x3))"
echo

# ---------------------------------------------------------------------------
# 3. Ingest expressions into a combined DB
# ---------------------------------------------------------------------------
echo "--- Step 3: Ingest expressions ---"
db_tool ingest \
  --db "$WORKDIR/combined.db" \
  --expressions "$WORKDIR/expressions.txt" \
  --dataset demo \
  --format TIR \
  --varnames "x0,x1,x2,t0,t1,t2" \
  --quiet

echo "  Created combined.db (egraph + fit in one file)"
db_tool status \
  --egraph "$WORKDIR/combined.db" \
  --fitdb "$WORKDIR/combined.db" \
  --dataset demo
echo

# ---------------------------------------------------------------------------
# 4. Convert to split-DB format
# ---------------------------------------------------------------------------
echo "--- Step 4: Convert to split-DB format ---"
python3 "tools/convert_db.py" \
  --input "$WORKDIR/combined.db" \
  --egraph "$WORKDIR/egraph.db" \
  --fit-prefix "$WORKDIR/fit_"

echo "  egraph.db: $(du -h "$WORKDIR/egraph.db" | cut -f1) (structural e-graph)"
echo "  fit_demo.db: $(du -h "$WORKDIR/fit_demo.db" | cut -f1) (per-dataset fitness)"
echo

# ---------------------------------------------------------------------------
# 5. Run equality saturation on the egraph
# ---------------------------------------------------------------------------
echo "--- Step 5: Run equality saturation (2 steps) ---"
db_tool eqsat \
  --db "$WORKDIR/egraph.db" \
  --dataset demo \
  --steps 3

echo "  Eqsat complete"
echo

# ---------------------------------------------------------------------------
# 6. Fit expressions against the dataset
# ---------------------------------------------------------------------------
echo "--- Step 6: Fit expressions ---"
db_tool fitdata \
  --egraph "$WORKDIR/egraph.db" \
  --fitdb "$WORKDIR/fit_demo.db" \
  --dataset demo \
  --data "$WORKDIR/data.csv:::y:x0,x1,x2" \
  --loss "NLL Gaussian" \
  --n-rep 1 \
  --n-iter 10 \
  --batch-size 500 \
  --quiet

echo "  Status after fitting:"
db_tool status \
  --egraph "$WORKDIR/egraph.db" \
  --fitdb "$WORKDIR/fit_demo.db" \
  --dataset demo
echo

# ---------------------------------------------------------------------------
# 7. Export all expressions (including NaN)
# ---------------------------------------------------------------------------
echo "--- Step 7: Export ALL expressions (including NaN) ---"
db_tool export \
  --egraph "$WORKDIR/egraph.db" \
  --fitdb "$WORKDIR/fit_demo.db" \
  --dataset demo \
  > "all_expressions.csv" 2>"$WORKDIR/export_all.log"

ALL_COUNT=$(tail -1 "$WORKDIR/export_all.log" | grep -oP '\d+')
echo "  Exported $ALL_COUNT expressions to all_expressions.csv"
echo "  Header:"
head -1 "all_expressions.csv"
echo "  Sample (first 5 non-NaN):"
grep -v NaN "all_expressions.csv" | head -6 | tail -5 | sed 's/^/    /'
NaN_COUNT=$(grep -c NaN "$WORKDIR/all_expressions.csv" || true)
echo "  NaN count: $NaN_COUNT"
echo

# ---------------------------------------------------------------------------
# 8. Export only finite (non-NaN) expressions
# ---------------------------------------------------------------------------
echo "--- Step 8: Export FINITE expressions only ---"
db_tool export \
  --egraph "$WORKDIR/egraph.db" \
  --fitdb "$WORKDIR/fit_demo.db" \
  --dataset demo \
  --finite \
  > "finite_expressions.csv" 2>"$WORKDIR/export_finite.log"

FINITE_COUNT=$(tail -1 "$WORKDIR/export_finite.log" | grep -oP '\d+')
echo "  Exported $FINITE_COUNT finite expressions to finite_expressions.csv"
echo "  Header:"
head -1 "finite_expressions.csv"
echo "  Sample (first 5):"
head -6 "finite_expressions.csv" | tail -5 | sed 's/^/    /'
echo

# ---------------------------------------------------------------------------
# 9. Summary
# ---------------------------------------------------------------------------
echo "=== Summary ==="
echo "  Expressions generated:  $TOTAL_EXPR"
echo "  Exported (all):         $ALL_COUNT"
echo "  Exported (finite):      $FINITE_COUNT"
echo "  NaN/invalid:            $NaN_COUNT"
echo
echo "  Split-DB files:"
echo "    $WORKDIR/egraph.db                - structural e-graph (shared)"
echo "    $WORKDIR/fit_demo.db              - fitness data for 'demo' dataset"
echo
echo "  Output files:"
echo "    $WORKDIR/all_expressions.csv      - all expressions (incl. NaN)"
echo "    $WORKDIR/finite_expressions.csv   - valid expressions only"
echo
echo "Done!"
