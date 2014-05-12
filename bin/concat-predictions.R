#!/usr/bin/env Rscript
#
# Usage:
#   ./bin/concat-predictions c50_M30_pred.txt c50_M31_pred.txt ensemble/M/M3_group3_c50_binary.txt
#   ./bin/concat-predictions c50_M30_prob.txt c50_M31_prob.txt ensemble/M/M3_group3_c50_confidence.txt

args <- commandArgs(T)

probs <- "-p" %in% args


suffix <- if ("-p" %in% args) "_prob" else "_pred"

p1 <- args[[1]]
message(paste("Using", p1))
p2 <- args[[2]]
message(paste("Using", p2))

dest <- args[[3]]

preds <- rbind(
    read.table(p1, sep=";", header=T),
    read.table(p2, sep=";", header=T))
preds <- preds[order(preds$orderItemID), ]
if (!all(preds$orderItemID == min(preds$orderItemID):max(preds$orderItemID)))
    warning("OrderItemIDs are not a continuous range!")

options(scipen=100)

write.table(preds, dest, sep=";", quote=F, row.names=F)
