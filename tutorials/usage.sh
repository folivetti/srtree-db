#!/usr/bin/env bash
#
# Installation instructions:
# - install ghcup https://www.haskell.org/ghcup/
# - clone repository `git clone https://github.com/folivetti/srtree-db.git`
# - in srtree-db directory `cabal install`
# - in tutorials directory run the following commands to test it:

# insert expressions into an e-graph DB
srtree-db ingest --db tutorial.db --format TIR --expressions expressions.txt
# run 3 steps of eq-sat
srtree-db eqsat --db tutorial.db --steps 3 --dataset demo
# fit the dataset and store it in fit_demo.db
srtree-db fitdata --egraph tutorial.db --fitdb fit_demo.db --dataset demo --data "data.csv:::y:x0,x1,x2" --loss "NLL Gaussian" --n-rep 5 --n-iter 10 --batch-size 500 
# export all the evaluated expressions
srtree-db export --egraph tutorial.db --fitdb fit_demo.db --dataset demo > all_expressions
# export all the evaluated and finite expressions
srtree-db export --egraph tutorial.db --fitdb fit_demo.db --dataset demo --finite > finite_expressions

