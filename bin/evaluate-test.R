#!/usr/bin/env Rscript
source("R/dmc.R")
dmc.reload()
library(plyr)
library(C50)
library(klaR)
library(MASS)
library(randomForest)

real <- read.csv("task2010/dmc2010_real.txt", sep=";")
colnames(real) <- c("customernumber", "target90")
real$target90 <- revalue(as.factor(real$target90), c("1"="yes", "0"="no"))

preds <- lapply(mds, function(x) merge(data.frame(customernumber=dt.test$customernumber,
                                            target90=predict(x, dt.test)),
                                       real, by="customernumber"))
points <- lapply(preds, function(x) dmc.points(x$target90.x, x$target90.y))

points
