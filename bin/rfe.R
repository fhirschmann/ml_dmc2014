#!/usr/bin/env Rscript

library(caret)
library(data.table)

source("R/data.R")
source("R/fs.R")

args <- commandArgs(T)
set.seed(42)

which <- args[[1]]

# 1 -> gbm with customer info
# 0 -> gbm without customer info
if (which == '1') {
    dt <- fs.gbm(data.frame(dt.dmc$M1$train))
} else {
    dt <- fs.noCustomer(fs.gbm(data.frame(dt.dmc$M1$train)))
}

set.seed(42)
dt <- dt[sample(nrow(dt), 100000), ]

zeroVar <- names(which(sapply(dt, function(x) length(unique(x)) == 1)))
dt <- dt[!names(dt) %in% zeroVar]

funcs <- caretFuncs
funcs$fit <- function(a, b, first, last, ...) {
    train(a, b, method = "gbm", tuneLength=1, ...)
}

rctrl <- rfeControl(functions = funcs, method="cv", number = 10,
                    saveDetails = T, allowParallel = F)
pred <- dt$returnShipment
dt$returnShipment <- NULL

rfefit <- rfe(x=dt, y=pred, 
              sizes = 10:20 * 2,
              metric="Accuracy",
              maximize = T,
              rfeControl=rctrl)
rfefit

saveRDS(rfefit, paste("rfe", which, ".RData", sep="")
