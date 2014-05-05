#!/usr/bin/env Rscript

source("R/data.R")

check <- function(cond, msg) {
    message(paste("Checking:", msg))
    if (!cond) {
        error("FAILED")
    }
}

for (s in names(dt.dmc)) {
    message(paste("Checking Set", s))
    oid.train <- as.numeric(as.character(read.csv(paste("eva/", s, "_train.txt", sep=""))$orderItemID))
    oid.test <- as.numeric(as.character(read.csv(paste("eva/", s, "_test.txt", sep=""))$orderItemID))
    
    check(all(dt.dmc[[s]]$train$orderItemID == oid.train), "Train Set: Matching OrderItemID")
    check(all(dt.dmc[[s]]$test$orderItemID == oid.test), "Test Set: Matching OrderItemID")
    
}