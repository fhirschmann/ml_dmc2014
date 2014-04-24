#!/usr/bin/env Rscript
source("R/data.R")
source("R/fs.R")
library(C50)

fit.c50.t1 <- C5.0(returnShipment ~ ., rules=TRUE, data=fs.all(dt.t1$train))