#!/usr/bin/env Rscript
library(caret)
library(data.table)

source("R/data.R")
source("R/fs.R")

dt.train <- data.frame(fs.all(dt.dmc$T1$train))
dt.test <- data.frame(fs.all(dt.dmc$T1$test))

# Take a sample
set.seed(42)
dt.train <- dt.train[sample(nrow(dt.train), 100), ]
zeroVar <- names(which(sapply(dt.train, function(x) length(unique(x)) == 1)))
dt.train <- dt.train[, !names(dt.train) %in% zeroVar]

set.seed(42)
dt.test <- dt.test[sample(nrow(dt.test), 10), ]
dt.test <- dt.test[, !names(dt.test) %in% zeroVar]

# Outcome
y.train <- dt.train$returnShipment
y.test <- dt.test$returnShipment

# Training Data
x.train <- dt.train
x.train$returnShipment <- NULL
x.test <- dt.test
x.test$returnShipment <- NULL

funcs <- caretFuncs
funcs$fit <- function(a, b, first, last, ...) {
    train(a, b, method = "gbm", tuneLength=1, ...) 
}

rctrl <- rfeControl(functions=funcs, saveDetails=T,
                    allowParallel=F)

set.seed(42)
rfefit <- rfeIter(x.train, y.train, x.test, y.test, sizes=4*(3:6),
                  rfeControl=rctrl)
rfefit
saveRDS(rfefit, file="rfe.RData")
