#!/usr/bin/env Rscript

#library(devtools)
#install_github("imputation", "jeffwong")
library(imputation)

source("R/data.R")
source("R/fs.R")

cut <- nrow(dt.train)
dt.train$returnShipment <- NULL
dt.train$holiday <- NULL

data <- fs.nb(rbind(dt.train, dt.test))
data$color <- as.character(data$color)
data$fewcolors <- as.character(data$fewcolors)

data[which(is.na(data$color)), ]$color <- "NA"
data[which(is.na(data$fewcolors)), ]$fewcolor <- "NA"

data$color <- as.factor(data$color)
data$fewcolors <- as.factor(data$fewcolors)

nas <- sapply(data, function(x) any(is.na(x)))
names(which(nas))

data.imp <- gbmImpute(data[1:1001,])