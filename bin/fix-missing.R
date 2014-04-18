#!/usr/bin/env Rscript
library(caret)
library(lubridate)
library(doMC)
library(digest)
library(plyr)
library(data.table)
library(Metrics)
library(data.table)
registerDoMC(cores=4)

source("R/data.R")
source("R/utils.R")

# fix creationDate

dt.customer <- dt.merged[c("customerID", "creationDate", "firstOrderDate", "orderDate", "price")]
dt.tbl <- data.table(dt.customer)
dt.customer$numberOfItemsOrdered <- dt.tbl[,x := .N, by=c("customerID")]$x
dt.customer$numberOfOrders <- dt.tbl[, x := .N, by=c("customerID", "orderDate")]$x
dt.customer$orderVolume <- dt.tbl[, x := as.integer(sum(price)), by=c("customerID")]$x


dt.customer$orderDate <- NULL
dt.customer$price <- NULL
dt.customer <- unique(dt.customer)
dt.customer <- dt.customer[!is.na(dt.customer$creationDate), ]
dt.customer$customerID <- as.numeric(dt.customer$customerID)
dt.customer$creationDate <- as.numeric(dt.customer$creationDate)
dt.customer$firstOrderDate <- as.numeric(dt.customer$firstOrderDate)



dt.customer.samp <- dt.customer[sample(nrow(dt.customer), nrow(dt.customer) / 10), ]

set.seed(100)

fit <- train(creationDate ~ .,
             method="cubist",
             data=dt.customer.samp[c("customerID", "creationDate", "firstOrderDate",
                                     "numberOfOrders", "numberOfItemsOrdered", "orderVolume")],
             metric="MAE",
             tuneGrid=expand.grid(.committees=1, .neighbors=0),
             trControl=trainControl(
                 method="cv",
                 summaryFunction=function(data, lev=NULL, model=NULL) c(MAE=mae(data$obs, data$pred))))
fit