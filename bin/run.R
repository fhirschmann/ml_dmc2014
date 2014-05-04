#!/usr/bin/env Rscript
#
# This runs the pipeline.
#
# You can give specify the models to train by giving
# its names as arguments, e.g. ./bin/run.R c50 rf
#
# To get a list of available models try ./bin/run.R -l

source("R/dmc.R")
source("R/pipeline.R")
source("R/utils.R")
library(doParallel)
registerDoParallel(1)

args <- commandArgs(T)

# Set the description for the learner
desc <- list.update(common.desc, descs[[args[[1]]]])

desc$data <- readRDS("data.dmc.RData")[[args[[2]]]]

if ("-m" %in% args) {
    desc$data <- list(train=head(desc$data$train, 2000),
                      test=head(desc$data$test, 100))
}

desc$data.name <- args[[2]]

system.time(train <- do.call(dmctrain, desc))
train[c("results", "bestResults")]
