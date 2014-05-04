#!/bin/bash
# Usage: ./bin/runmany.sh c50 T1 T2 T3

L=$1
shift

for i in $*; do
    ./bin/run.R $L $i
done
