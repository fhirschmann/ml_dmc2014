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

if("-B" %in% args) {
	##for bag evaluation only:
	delayedAssign("dt.train", readRDS("bagdata.train.RData"))
	delayedAssign("dt.test", readRDS("bagdata.test.RData"))
	delayedAssign("dt.dmc", readRDS("bagdata.dmc.RData"))
	delayedAssign("dt.dmc.ids", readRDS("bagdata.dmc.ids.RData"))
}

desc$data <- dt.dmc[[args[[2]]]]

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
} else if ("-x" %in% args) {
    desc$data <- list(train=desc$data$train[sample(nrow(desc$data$train), 100000), ],
                      test=desc$data$test[sample(nrow(desc$data$test), 10000), ])
    desc$save.path <- "models.x"
} else if ((length(args) > 2 & !("-B" %in% args)) | (length(args) > 3)) {
    message(paste("Enabling", args[[3]], "cores"))
    library(doMC)
    registerDoMC(as.integer(args[[3]]))
}

if("-fewermonths" %in% args) {
    desc$data$train$month <- ((year(desc$data$train$orderDate) - 2012) * 12 + month(desc$data$train$orderDate)) - 3
    desc$data$train <- desc$data$train[desc$data$train$month != 3 
                                       & desc$data$train$month != 4 
                                       & desc$data$train$month != 7
                                       & desc$data$train$month != 8, ]
    desc$data$train$month <- NULL
    desc$save.path <- "models.fewermonths"
}

desc$data.name <- args[[2]]
desc$name <- args[[1]]

if (file.exists("config.R")) source("config.R")

if (!is.null(desc$save.path))
    if (!file.exists(desc$save.path))
        dir.create(desc$save.path)

if (!is.null(desc$save.path)) message(paste("Writing to directory", desc$save.path))

system.time(train <- do.call(dmctrain, desc))
train[setdiff(names(train), c("model", "pred"))]
warnings()

# Upload results

if ((Sys.getenv("USER") %in% c("io93bafi", "ex66pimo")) | (Sys.info()["nodename"] %in% c("ip-172-31-7-33", "ip-172-31-16-9"))) {
    system("./bin/sync-cluster.sh")
}
