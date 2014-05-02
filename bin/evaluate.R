#!/usr/bin/Rscript
#
# Usage: (Rscript) bin/evaluate.R models/
source("R/dmc.R")

args <- commandArgs(T)

dmc.loadeva(args[[1]])
