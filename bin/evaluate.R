#!/usr/bin/Rscript
#
# Usage: (Rscript) bin/evaluate.R models
library(stringr)

source("R/dmc.R")

args <- commandArgs(T)
eva <- dmc.evaluate(args[[1]])
eva