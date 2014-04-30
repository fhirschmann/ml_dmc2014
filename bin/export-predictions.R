#!/usr/bin/env Rscript
#
# Usage: ./bin/export-predictions.R models/C5.0.RData directory/
#        ./bin/export-predictions.R models/C5.0.RData file.txt

source("R/dmc.R")
library(stringr)

args <- commandArgs(T)

fit <- readRDS(args[[1]])
preds <- sapply(extractPreds.dmcmtrain(fit),
                function(x) {
                    x$prediction <- dmc.convertPreds(x$prediction)
                    x
                }, simplify=F)

if (str_sub(args[[2]], -4, -1) == ".txt") {
    pred <- do.call(rbind, preds)
    write.table(pred, file=args[[2]], quote=F, row.names=F, sep=";")
} else {
    for (n in names(preds)) {
        write.table(preds[[n]], file=file.path(args[[2]], paste(n, "_pred.txt", sep="")),
                    quote=F, row.names=F, sep=";")   
    }
}