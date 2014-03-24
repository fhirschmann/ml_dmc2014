#!/usr/bin/env Rscript
source("R/dmc.R")

eva <- dmc.evaluate.test()
write.csv(eva, file="doc/results_test.csv")

eva
