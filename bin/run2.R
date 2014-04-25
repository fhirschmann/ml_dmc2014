#!/usr/bin/env Rscript
source("R/data.R")
source("R/fs.R")
source("R/dmc.R")
library(C50)

fit.c50 <- dmc.train(C5.0, fs.fun=fs.tree)
caret.save(fit.c50, "c50", "models2")
dmc.evaluate(fit.c50)