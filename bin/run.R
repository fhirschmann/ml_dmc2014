#!/usr/bin/env Rscript
#
# This runs the pipeline

source("R/dmc.R")
dmc.reload()
try.mp()

args <- commandArgs(T)


if (length(args) == 0) {
    # Train all models
    totrain <- names(descs)
} else {
    totrain <- args
}

cat("Training the following models:", totrain, "\n")

for (name in totrain) {
    print(dmc.run(name))
}
