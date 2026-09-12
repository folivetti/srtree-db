#!/usr/bin/env bash
srtree-db ingest --db tutorial.db --format TIR --expressions expressions.txt
srtree-db eqsat --db tutorial.db --steps 3 --dataset demo        
srtree-db fitdata --egraph tutorial.db --fitdb fit_demo.db --dataset demo --data "data.csv:::y:x0,x1,x2" --loss "NLL Gaussian" --n-rep 5 --n-iter 10 --batch-size 500 
srtree-db export --egraph tutorial.db --fitdb fit_demo.db --dataset demo > all_expressions
srtree-db export --egraph tutorial.db --fitdb fit_demo.db --dataset demo --finite > finite_expressions

