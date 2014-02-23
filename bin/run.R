#!/usr/bin/env Rscript
#
# This runs the pipeline

source("R/common.R")
source("R/pipeline.R")

args <- commandArgs(T)

run <- function(name) {
    fit <- trainers[[name]]()
        
    fname <- file.path("models", paste(name, ".RData", sep=""))
    save(fit, file=fname)
    
    cat("Wrote model for", name, "to", fname, "\n")
    print(fit)
}

if (length(args) == 0) {
    totrain <- names(trainers)
} else {
    totrain <- args
}

cat("Training the following models:", totrain, "\n")

for (name in totrain) {
    run(name)
}