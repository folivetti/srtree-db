#!/usr/bin/env python3
"""Generate 1000 random expressions in OPERON format for the srtree-db export tutorial.

Some expressions are intentionally invalid (sqrt of negative, log of negative,
division by zero) to demonstrate the pruning and export workflow.

Usage:
    python3 gen_expressions.py > expressions.txt
"""
import random
import sys

VARS = ["x0", "x1", "x2", "t0", "t1", "t2"]
UNARY_OPS = [
    ("sqrt", "sqrt"),   # can produce NaN for negative input
    ("log", "log"),     # can produce NaN for non-positive input
    ("exp", "exp"),     # can overflow
    ("abs", "abs"),     # always valid
    ("sin", "sin"),     # always valid
    ("cos", "cos"),     # always valid
    ("square", "square"), # always valid
]
BIN_OPS = [
    ("+", "+"),
    ("-", "-"),
    ("*", "*"),
    ("/", "/"),  # can produce NaN/Inf for zero divisor
]

random.seed(42)

def gen_expr(depth=0, max_depth=4):
    """Generate a random expression tree."""
    if depth >= max_depth or (depth > 0 and random.random() < 0.3):
        # Leaf: variable or constant
        if random.random() < 1.0:
            return random.choice(VARS)
        else:
            # Constants that might cause issues
            c = random.choice([-2.0, -1.0, 0.0, 0.5, 1.0, 2.0, 3.0])
            if c == int(c):
                return str(int(c))
            return f"{c:.2f}"

    if random.random() < 0.4:
        # Unary operation
        op_name, op_sym = random.choice(UNARY_OPS)
        arg = gen_expr(depth + 1, max_depth)
        return f"{op_sym}({arg})"
    else:
        # Binary operation
        op_name, op_sym = random.choice(BIN_OPS)
        left = gen_expr(depth + 1, max_depth)
        right = gen_expr(depth + 1, max_depth)
        return f"({left} {op_sym} {right})"

def main():
    n = int(sys.argv[1]) if len(sys.argv) > 1 else 1000
    expressions = set()
    attempts = 0
    while len(expressions) < n and attempts < n * 10:
        expr = gen_expr()
        # Skip trivially duplicate expressions
        if expr not in expressions:
            expressions.add(expr)
        attempts += 1

    for expr in sorted(expressions):
        print(expr)

    # Print stats to stderr
    total = len(expressions)
    sqrt_count = sum(1 for e in expressions if "sqrt(" in e)
    log_count = sum(1 for e in expressions if "log(" in e)
    div_count = sum(1 for e in expressions if "/" in e)
    print(f"# Generated {total} expressions ({sqrt_count} sqrt, {log_count} log, {div_count} division)", file=sys.stderr)

if __name__ == "__main__":
    main()
