#!/usr/bin/env Rscript
#
# This runs the pipeline.
#
# You can give specify the models to train by giving
# its names as arguments, e.g. ./bin/run.R c50 rf
#
# To get a list of available models try ./bin/run.R -l

source("R/data.R")
source("R/dmc.R")
source("R/pipeline.R")
source("R/utils.R")
library(doParallel)
registerDoParallel(1)


# RAM...
dt.train <- NULL
dt.test <- NULL
dt.dmc <- NULL
gc(F)


args <- commandArgs(T)

dtrain <- dmcdtrain(descs[[args[[1]]]], common.desc)
dtrain[c("results", "bestResults")]