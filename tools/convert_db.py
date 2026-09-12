#!/usr/bin/env python3
"""
Convert a single combined srtree-db database into the split format:
  - egraph.db: egraph tables (meta, enode, enode_child, eclass, eclass_node, cstore_page, frontier, dataset)
  - fit_<dataset>.db: per-dataset tables (dataset, dataset_fit, expression_index)

Uses ATTACH + INSERT...SELECT so data never enters Python memory — works on
databases of any size without OOM.

Usage:
  python3 convert_db.py --input old.db --egraph egraph.db --fit-prefix fit_
"""

import argparse
import os
import sqlite3
import sys
import time

BATCH_SIZE = 50000  # rows per INSERT...SELECT batch (keeps memory bounded)


def convert(input_path, egraph_path, fit_prefix):
    if not os.path.exists(input_path):
        print(f"Error: input file {input_path} not found")
        sys.exit(1)

    src = sqlite3.connect(input_path)
    src.execute("PRAGMA journal_mode=WAL")
    src.execute("PRAGMA synchronous=NORMAL")

    # --- Phase 1: Create egraph DB (attach + copy) ---
    print(f"Creating egraph DB: {egraph_path}")
    if os.path.exists(egraph_path):
        os.remove(egraph_path)
    src.execute(f"ATTACH DATABASE '{egraph_path}' AS egraph_db")
    _attach_egraph_schema(src, "egraph_db")

    # Copy egraph tables one by one using INSERT...SELECT (streamed by SQLite)
    egraph_tables = [
        "meta", "enode", "enode_child", "eclass", "eclass_node",
        "cstore_page", "frontier", "dataset"
    ]
    for table in egraph_tables:
        _copy_table_streaming(src, table, "egraph_db")

    src.commit()
    src.execute("DETACH DATABASE egraph_db")
    print(f"  Egraph DB created: {_file_size(egraph_path)}")

    # --- Phase 2: Create fit DBs (one per dataset) ---
    datasets = []
    try:
        for row in src.execute("SELECT id, name FROM dataset"):
            datasets.append((row[0], row[1]))
    except sqlite3.OperationalError:
        print("Warning: no dataset table found")

    if not datasets:
        print("No datasets to convert.")
        src.close()
        return

    print(f"Found {len(datasets)} dataset(s)")
    for ds_id, ds_name in datasets:
        fit_path = f"{fit_prefix}{ds_name}.db"
        print(f"Creating fit DB for '{ds_name}': {fit_path}")
        if os.path.exists(fit_path):
            os.remove(fit_path)
        _create_fit_db(src, fit_path, ds_id)

    src.close()
    print("Done.")


def _attach_egraph_schema(src, alias):
    """Create the egraph schema in the attached DB."""
    src.executescript(f"""
        CREATE TABLE {alias}.meta (
            key TEXT PRIMARY KEY,
            value TEXT NOT NULL);
        CREATE TABLE {alias}.enode (
            key TEXT PRIMARY KEY,
            op TEXT NOT NULL,
            op_detail TEXT,
            a INTEGER, b INTEGER, x REAL);
        CREATE TABLE {alias}.enode_child (
            enode_key TEXT NOT NULL,
            child_eid INTEGER NOT NULL,
            cnt INTEGER NOT NULL DEFAULT 1,
            PRIMARY KEY (enode_key, child_eid));
        CREATE TABLE {alias}.eclass (
            eid INTEGER PRIMARY KEY,
            canonical INTEGER NOT NULL,
            height INTEGER NOT NULL DEFAULT 0);
        CREATE TABLE {alias}.eclass_node (
            eid INTEGER NOT NULL,
            enode_key TEXT NOT NULL,
            PRIMARY KEY (eid, enode_key));
        CREATE TABLE {alias}.cstore_page (
            key INTEGER PRIMARY KEY,
            blob BLOB NOT NULL);
        CREATE TABLE {alias}.frontier (
            eid INTEGER PRIMARY KEY,
            updated_at TEXT);
        CREATE TABLE {alias}.dataset (
            id INTEGER PRIMARY KEY,
            name TEXT NOT NULL UNIQUE,
            created TEXT);
    """)


def _copy_table_streaming(src, table, alias):
    """Copy a table using INSERT...SELECT — data stays in SQLite, never in Python."""
    try:
        # Count rows for progress
        count = src.execute(f"SELECT COUNT(*) FROM {table}").fetchone()[0]
        if count == 0:
            print(f"  {table}: 0 rows (empty)")
            return

        # Get column names from source
        cols = [row[1] for row in src.execute(f"PRAGMA table_info({table})").fetchall()]
        col_list = ", ".join(cols)

        # Copy in batches using INSERT...SELECT with LIMIT/OFFSET
        copied = 0
        t0 = time.time()
        while copied < count:
            batch = min(BATCH_SIZE, count - copied)
            src.execute(
                f"INSERT INTO {alias}.{table} ({col_list}) "
                f"SELECT {col_list} FROM {table} LIMIT ? OFFSET ?",
                (batch, copied)
            )
            copied += batch
            elapsed = time.time() - t0
            rate = copied / elapsed if elapsed > 0 else 0
            print(f"\r  {table}: {copied}/{count} rows ({rate:.0f} rows/s)", end="", flush=True)
        print(f"  {table}: {count} rows copied in {time.time()-t0:.1f}s")
    except sqlite3.OperationalError as e:
        print(f"  Warning: could not copy {table}: {e}")


def _create_fit_db(src, fit_path, ds_id):
    """Create a fit DB for one dataset using ATTACH + INSERT...SELECT."""
    alias = f"fit_db_{ds_id}"
    src.execute(f"ATTACH DATABASE '{fit_path}' AS {alias}")

    # Create schema
    src.executescript(f"""
        CREATE TABLE {alias}.dataset (
            id INTEGER PRIMARY KEY,
            name TEXT NOT NULL UNIQUE,
            created TEXT
        );
        CREATE TABLE {alias}.dataset_fit (
            dataset_id INTEGER NOT NULL,
            eid INTEGER NOT NULL,
            fitness REAL, dl REAL, theta TEXT,
            size INTEGER NOT NULL DEFAULT 0,
            evaluated INTEGER NOT NULL DEFAULT 0,
            fitted INTEGER NOT NULL DEFAULT 0,
            stale INTEGER NOT NULL DEFAULT 0,
            updated_at TEXT,
            PRIMARY KEY (dataset_id, eid)
        );
        CREATE TABLE {alias}.expression_index (
            expression_key TEXT PRIMARY KEY,
            eclass INTEGER NOT NULL,
            dataset_id INTEGER,
            first_seen TEXT
        );
    """)

    # Copy dataset row
    src.execute(
        f"INSERT INTO {alias}.dataset SELECT * FROM dataset WHERE id = ?",
        (ds_id,)
    )

    # Copy dataset_fit rows for this dataset in batches
    count = src.execute(
        "SELECT COUNT(*) FROM dataset_fit WHERE dataset_id = ?", (ds_id,)
    ).fetchone()[0]
    if count > 0:
        copied = 0
        t0 = time.time()
        while copied < count:
            batch = min(BATCH_SIZE, count - copied)
            src.execute(
                f"INSERT INTO {alias}.dataset_fit "
                f"SELECT dataset_id, eid, fitness, dl, theta, size, evaluated, fitted, stale, updated_at "
                f"FROM dataset_fit WHERE dataset_id = ? LIMIT ? OFFSET ?",
                (ds_id, batch, copied)
            )
            copied += batch
            elapsed = time.time() - t0
            rate = copied / elapsed if elapsed > 0 else 0
            print(f"\r  dataset_fit: {copied}/{count} rows ({rate:.0f} rows/s)", end="", flush=True)
        print(f"  dataset_fit: {count} rows copied in {time.time()-t0:.1f}s")

    # Copy expression_index rows for this dataset
    try:
        count = src.execute(
            "SELECT COUNT(*) FROM expression_index WHERE dataset_id = ?", (ds_id,)
        ).fetchone()[0]
        if count > 0:
            copied = 0
            t0 = time.time()
            while copied < count:
                batch = min(BATCH_SIZE, count - copied)
                src.execute(
                    f"INSERT INTO {alias}.expression_index "
                    f"SELECT expression_key, eclass, dataset_id, first_seen "
                    f"FROM expression_index WHERE dataset_id = ? LIMIT ? OFFSET ?",
                    (ds_id, batch, copied)
                )
                copied += batch
                elapsed = time.time() - t0
                rate = copied / elapsed if elapsed > 0 else 0
                print(f"\r  expression_index: {copied}/{count} rows ({rate:.0f} rows/s)", end="", flush=True)
            print(f"  expression_index: {count} rows copied in {time.time()-t0:.1f}s")
    except sqlite3.OperationalError:
        print("  Warning: no expression_index table")

    src.commit()
    src.execute(f"DETACH DATABASE {alias}")
    print(f"  Fit DB created: {_file_size(fit_path)}")


def _file_size(path):
    """Human-readable file size."""
    size = os.path.getsize(path)
    for unit in ['B', 'KB', 'MB', 'GB']:
        if size < 1024:
            return f"{size:.1f}{unit}"
        size /= 1024
    return f"{size:.1f}TB"


def main():
    parser = argparse.ArgumentParser(description="Convert single srtree-db to split format")
    parser.add_argument("--input", required=True, help="Input combined database file")
    parser.add_argument("--egraph", required=True, help="Output egraph database file")
    parser.add_argument("--fit-prefix", default="fit_", help="Prefix for fit database files (default: fit_)")
    args = parser.parse_args()
    convert(args.input, args.egraph, args.fit_prefix)


if __name__ == "__main__":
    main()
