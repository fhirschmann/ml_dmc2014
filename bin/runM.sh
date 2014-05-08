#!/bin/bash

./bin/run.R gbmM1 M1 $* > M1.out 2>&1
./bin/run.R gbmM10 M10 $* >  M10.out 2>&1
./bin/run.R gbmM11 M11 $* > M11.out 2>&1
