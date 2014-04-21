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

# fix customerAge

dt.age <- dt.merged[c("customerID", "customerAge", "fewcolors", "orderDate", "price")]
dt.age.tbl <- data.table(dt.age)
dt.age$numberOfItemsOrdered <- dt.age.tbl[,x := .N, by=c("customerID")]$x
#dt.age$numberOfOrders <- dt.age.tbl[, x := .N, by=c("customerID", "orderDate")]$x
dt.age$orderVolume <- dt.age.tbl[, x := as.integer(sum(price)), by=c("customerID")]$x
dt.age$favoriteColor <- names(which.max(prop.table(table(dt.age$fewcolors))))
dt.age$favoriteColor <- as.factor(dt.age$favoriteColor)
dt.age$leastFavoriteColor <- names(which.min(prop.table(table(dt.age$fewcolors))))
dt.age$leastFavoriteColor <- as.factor(dt.age$leastFavoriteColor)

dt.age$fewcolors <- NULL
dt.age$orderDate <- NULL
dt.age$price <- NULL

dt.age <- unique(dt.age[!is.na(dt.age$customerAge), ])

dt.age.samp <- dt.age[sample(nrow(dt.age), nrow(dt.age) / 10), ]

set.seed(100)
fit <- train(customerAge ~ .,
             method="cubist",
             data=dt.age,
             metric="MAE",
             tuneGrid=expand.grid(.committees=1, .neighbors=0),
             trControl=trainControl(
                 method="cv",
                 summaryFunction=function(data, lev=NULL, model=NULL) c(MAE=mae(data$obs, data$pred))))
fit


