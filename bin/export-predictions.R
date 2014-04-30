#!/usr/bin/env Rscript

source("R/dmc.R")
args <- commandArgs(T)

fit <- readRDS(args[[1]])
preds <- extractPreds.dmcmtrain(fit)

for (n in names(preds)) {
    pred <- preds[[n]]
    pred$prediction <- dmc.convertPreds(pred$prediction)
    write.table(pred, file=file.path(args[[2]], paste(n, "_pred.txt", sep="")),
                quote=F, row.names=F, sep=";")
}