#!/usr/bin/env Rscript
# Usage:
#   ./bin/final-predict.R models/ c50 pred.txt
source("R/data.R")
library(caret)
library(C50)
library(earth)
library(plyr)

args <- commandArgs(T)
dir <- args[[1]]
name <- args[[2]]
out <- args[[3]]

# Comment this out for the final predictions
sp <- "M3"
# sp <- "F"

f1.file <- file.path(dir, paste(name, "_", sp, "1", ".RData", sep=""))
f0.file <- file.path(dir, paste(name, "_", sp, "0", ".RData", sep=""))

message(paste("Using", f1.file, "to predict known customers."))
message(paste("Using", f0.file, "to predict unknown customers."))

f1.fit <- readRDS(f1.file)
f0.fit <- readRDS(f0.file)

f1.pred <- predict(f1$model, dt.dmc$F1$test)
f0.pred <- predict(f0$model, dt.dmc$F0$test)

dt.f1 <- dt.dmc$F1$test
dt.f0 <- dt.dmc$F0$test

dt.f1$prediction <- predict(f1.fit, dt.f1)
dt.f0$prediction <- predict(f0.fit, dt.f0)

dt.f1[dt.f1$deliveryDateMissing == "yes", c("prediction")] <- "no"
dt.f0[dt.f0$deliveryDateMissing == "yes", c("prediction")] <- "no"

dt.f1$prediction <- revalue(dt.f1$prediction, c("no"=0, "yes"=1))
dt.f0$prediction <- revalue(dt.f0$prediction, c("no"=0, "yes"=1))

options(scipen=1000)

dt.f <- rbind(dt.f1, dt.f0)
dt.f <- dt.f[order(dt.f$orderItemID), ]


write.table(dt.f[, c("orderItemID", "prediction")], file=out, sep=";", quote=F)
