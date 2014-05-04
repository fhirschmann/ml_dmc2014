#!/usr/bin/env Rscript
#
# Usage: ./bin/export-predictions.R models/C5.0_T1.RData ... foo.txt
#        ./bin/export-predictions.R models/C5.0_T1.RData ... directory/

source("R/dmc.R")
library(stringr)

args <- commandArgs(T)

files <- args[1:length(args)-1]
out <- args[[length(args)]]
models <- sapply(files, readRDS, simplify=F)
names(models) <- sapply(files, function(x) str_split(str_sub(basename(x), 1, -7), "_")[[1]][[2]])
preds <- sapply(models, extractPreds.dmctrain, simplify=F)

if (str_sub(out, -4, -1) == ".txt") {
    message("Writing to file ", out)
    pred <- do.call(rbind, preds)
    write.table(pred, file=out, quote=F, row.names=F, sep=";")
} else {
    message("Writing to directory ", out)
    for (n in names(preds)) {
        write.table(preds[[n]], file=file.path(out, paste(n, "_pred.txt", sep="")),
                    quote=F, row.names=F, sep=";")   
    }
}