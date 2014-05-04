#!/usr/bin/Rscript
#
# Usage: (Rscript) bin/evaluate.R models
library(stringr)

source("R/dmc.R")

args <- commandArgs(T)
eva <- dmc.evaluate(args[[1]])
eva[c("results", "accuracy")]
cat("Wiki:\r\n")
print(eva$wiki, row.names=F)