#!/usr/bin/env Rscript
#
# This runs the pipeline

source("R/common.R")
source("R/pipeline.R")

args <- commandArgs(T)


if (length(args) == 0) {
    totrain <- names(trainers)
} else {
    totrain <- args
}

cat("Training the following models:", totrain, "\n")

for (name in totrain) {
    print(run(name))
}