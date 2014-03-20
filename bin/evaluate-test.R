#!/usr/bin/env Rscript
source("R/dmc.R")

eva <- dmc.evaluate(mds)
write.csv(eva, file="doc/results.csv")

eva