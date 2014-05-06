#!/usr/bin/env Rscript
#
# Usage: ./bin/run c50 T1 (-m for mini data set)

source("R/dmc.R")
source("R/pipeline.R")
source("R/utils.R")
library(doParallel)
registerDoParallel(1)

args <- commandArgs(T)

# Set the description for the learner
desc <- list.update(common.desc, descs[[args[[1]]]])

desc$data <- readRDS("data.dmc.RData")[[args[[2]]]]

set.seed(42)
if ("-m" %in% args) {
    desc$data <- list(train=desc$data$train[sample(nrow(desc$data$train), 2000), ],
                      test=desc$data$test[sample(nrow(desc$data$test), 100), ])
    desc$save.path <- "models.medium"
} else if ("-s" %in% args) {
    desc$data <- list(train=desc$data$train[sample(nrow(desc$data$train), 20000), ],
                      test=desc$data$test[sample(nrow(desc$data$test), 1000), ])
    desc$save.path <- "models.small"
} else if ("-t" %in% args) {
    desc$data <- list(train=desc$data$train[sample(nrow(desc$data$train), 100), ],
                      test=desc$data$test[sample(nrow(desc$data$test), 10), ])
    desc$save.path <- "models.tiny"
}

message(paste("Writing to directory", desc$save.path))

desc$data.name <- args[[2]]
desc$name <- args[[1]]

if (!file.exists(desc$save.path)) dir.create(desc$save.path)

system.time(train <- do.call(dmctrain, desc))
train[c("results", "bestResults", "method", "label")]
warnings()
