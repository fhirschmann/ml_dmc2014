#!/usr/bin/env Rscript
#
# Usage: ./bin/run.R c50 T1 (-m|-s|-t) (-d)
# Arguments:
#   -t Train on the Tiny Dataset
#   -s Train on the Small Dataset
#   -m Train on the Medium Dataset
#   -d Run ./bin/make-data.R

args <- commandArgs(T)

if ("-d" %in% args) {
    system("./bin/make-data.R")
}

source("R/data.R")
source("R/dmc.R")
source("R/pipeline.R")
source("R/utils.R")

if (length(args) < 2) {
    stop("Usage: ./bin/run.R c50 T1 (-m|-s|-t) (-d)")
}

#library(doParallel)
#registerDoParallel(1)

# Set the description for the learner
desc <- descs[[args[[1]]]]
if (is.null(desc)) stop(paste("Unknown Description. Available:",
                              paste(names(descs), collapse=", ")))
desc <- list.update(common.desc, descs[[args[[1]]]])

desc$data <- readRDS("data.dmc.RData")[[args[[2]]]]
if (is.null(desc$data)) stop("Unknown Data Set")

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

desc$data.name <- args[[2]]
desc$name <- args[[1]]

if (file.exists("config.R")) source("config.R")

if (!is.null(desc$save.path))
    if (!file.exists(desc$save.path))
        dir.create(desc$save.path)

if (!is.null(desc$save.path)) message(paste("Writing to directory", desc$save.path))

system.time(train <- do.call(dmctrain, desc))
train[c("results", "bestResults", "method", "label")]
warnings()
