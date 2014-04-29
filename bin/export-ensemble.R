#!/usr/bin/env Rscript

source("R/dmc.R")
args <- commandArgs(T)

fit <- readRDS(args[[1]])
preds <- extractPreds.dmctrain(fit)
write.table(preds, file=args[[2]], quote=F, sep=";",
            row.names=F)