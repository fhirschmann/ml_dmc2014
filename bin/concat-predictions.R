#!/usr/bin/env Rscript
#
# Usage:
#   ./bin/concat-predictions models c50C ensemble/M/M3_group3_c50_binary.txt
#   ./bin/concat-predictions models c50C ensemble/M/M3_group3_c50_confidence.txt -p

args <- commandArgs(T)

probs <- "-p" %in% args


suffix <- if ("-p" %in% args) "_prob" else "_pred"

dir <- args[[1]]
name <- args[[2]]
dest <- args[[3]]

p1 <- paste(file.path(dir, paste(name, "_", "M30", suffix, ".txt", sep="")))
p2 <- paste(file.path(dir, paste(name, "_", "M31", suffix, ".txt", sep="")))

preds <- rbind(
    read.table(p1, sep=";", header=T),
    read.table(p2, sep=";", header=T))

options(scipen=100)

write.table(preds, dest, sep=";", quote=F)