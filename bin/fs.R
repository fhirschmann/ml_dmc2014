#!/usr/bin/env Rscript
library(caret)
library(data.table)

source("R/data.R")
source("R/fs.R")

dt <- fs.all(dt.train)
set.seed(42)
dt <- dt[sample(nrow(dt), 100), ]

funcs <- caretFuncs
funcs$fit <- function(a, b, first, last, ...) {
    train(a, b, method = "gbm", tuneLength=1, ...) 
}

rctrl <- rfeControl(functions=funcs, method="cv", number=2,
                    saveDetails=T, allowParallel=F)

pred <- dt$returnShipment
dt$returnShipment <- NULL

set.seed(42)
rfefit <- rfe(x=dt, y=pred, sizes = 4*(3:6), 
              metric="Accuracy", maximize=T, rfeControl=rctrl)
rfefit
saveRDS(rfefit, file="rfe.RData")
