#!/usr/bin/Rscript
#
# Usage: (Rscript) bin/evaluate.R models
source("R/dmc.R")

args <- commandArgs(T)
dir <- args[[1]]
files <- list.files(path=dir, pattern=paste(pattern=".*_*._res.RData", sep=""))

res <- sapply(files, function(x) readRDS(file.path(dir, x)), simplify=F)
for (n in names(res)) {
    # TODO
}
res
