#!/usr/bin/env Rscript
source("R/data.R")
source("R/fs.R")
source("R/dmc.R")
source("R/utils.R")
try.mp()
library(C50)

fit.c50 <- DmcTrain(C5.0, fs.fun=fs.tree, rules=T, trials=1)
caret.save(fit.c50, "c50", "models2")
summary(fit.c50)