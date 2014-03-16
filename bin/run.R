#!/usr/bin/env Rscript
#
# This runs the pipeline.
#
# You can give specify the models to train by giving
# its names as arguments, e.g. ./bin/run.R c50 rf
#
# To get a list of available models try ./bin/run.R -l

source("R/pipeline.R")

args <- commandArgs(T)

if ("-l" %in% args) {
    message(paste(names(descs), collapse=", "))
    quit()
}

source("R/dmc.R")
dmc.reload()
try.mp()

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
