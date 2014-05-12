#!/usr/bin/env Rscript
# Usage:
#   ./bin/final-predict.R models/ c50 pred.txt
source("R/data.R")
library(caret)
library(C50)
library(earth)
library(plyr)

args <- commandArgs(T)
out <- args[[3]]

# Comment this out for the final predictions
#sp <- "M3"
# sp <- "F"

f1.file <- file.path(dir, paste(name, "_", sp, "1", ".RData", sep=""))
f0.file <- file.path(dir, paste(name, "_", sp, "0", ".RData", sep=""))

#message(paste("Using", f1.file, "to predict known customers."))
#message(paste("Using", f0.file, "to predict unknown customers."))
f0.file <- args[[1]]
f1.file <- args[[2]]

f1.fit <- readRDS(f1.file)
f0.fit <- readRDS(f0.file)

f1.dt <- dt.dmc$F1$test
f0.dt <- dt.dmc$F0$test

message("Predicting known customers")
f1.pred <- predict(f1.fit, f1.dt)
f1.dt$prediction <- f1.pred
message(paste("Predicted", length(f1.pred), "out of", nrow(f1.dt), "instances"))

message("Predicting unknown customers")
f0.pred <- predict(f0.fit, f0.dt)
f0.dt$prediction <- f0.pred
message(paste("Predicted", length(f0.pred), "out of", nrow(f0.dt), "instances"))

dt.f1[dt.f1$deliveryDateMissing == "yes", c("prediction")] <- "no"
dt.f0[dt.f0$deliveryDateMissing == "yes", c("prediction")] <- "no"

dt.f1$prediction <- revalue(dt.f1$prediction, c("no"=0, "yes"=1))
dt.f0$prediction <- revalue(dt.f0$prediction, c("no"=0, "yes"=1))

options(scipen=1000)

dt.f <- rbind(dt.f1, dt.f0)
dt.f <- dt.f[order(dt.f$orderItemID), ]


write.table(dt.f[, c("orderItemID", "prediction")], file=out, sep=";", quote=F)
