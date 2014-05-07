#!/usr/bin/env Rscript

library(caret)
library(data.table)

load('data.RData')
source("R/pipeline.R")
source("R/data.R")
source("R/fs.R")

dt <- fs.all(dt.train)

c50funcs <- caretFuncs
c50funcs$fit <- function(a, b, first, last, ...) { train(a, b, method = 'C5.0', ...) }
rctrl <- rfeControl(functions = c50funcs, method='cv', number = 10, saveDetails = T, allowParallel = F)
pred <- dt$returnShipment
dt$returnShipment <- NULL
rfe(x=dt,
    y=pred,
    sizes = 4*(3:6),
    metric="Accuracy",
    maximize = T,
    rfeControl=rctrl)
