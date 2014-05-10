#!/usr/bin/env Rscript
library(data.table)
library(plyr)
library(lubridate)
library(vcd)
library(zoo)
library(foreach)

args <- commandArgs(T)
schatti <- F

if (file.exists("config_mkdata.R")) source("config_mkdata.R")

source("R/feat.R")

if (schatti)
    source("R/featSchatti.R")

# Read in the Data
dt.na.strings <- c("NA", "", "??", "?")

dt.classes <- c(
    "orderItemID"="integer",
    "orderDate"="Date",
    "deliveryDate"="Date",
    "dateOfBirth"="Date",
    "itemID"="integer",
    "size"="factor",
    "manufacturerID"="integer",
    "customerID"="integer",
    "creationDate"="Date"
)

dt.train <- read.csv("task/orders_train.txt", sep=";",
                     colClasses=dt.classes, na.strings=dt.na.strings)
dt.train$returnShipment <- revalue(as.factor(dt.train$returnShipment), c("0"="no", "1"="yes"))
dt.train$holiday <- read.csv("task/orders_train.holiday.txt")$holiday
dt.test <- read.csv("task/orders_class.txt", sep=";",
                    colClasses=dt.classes, na.strings=dt.na.strings)
dt.test$holiday <- read.csv("task/orders_class.holiday.txt")$holiday

dt.train$returnShipment <- revalue(dt.train$returnShipment, c("0"="no", "1"="yes"))

rm.outliers <- function(dt) {
    dt2 <- dt
    
    # dateOfBirth/Age
    dt2$dateOfBirthMissing <- as.factor(ifelse(is.na(dt2$dateOfBirth), "yes", "no"))
    
    outliers <- with(dt2,
                     !is.na(dateOfBirth) 
                     & (dateOfBirth == as.Date("1949-11-19")
                        | as.integer(year(orderDate) - year(dateOfBirth)) > 85
                        | as.integer(year(orderDate) - year(dateOfBirth)) < 19))
    dt2[outliers, c("dateOfBirth")] <- NA
    dt2$dateOfBirthIsOutlier <- "no"
    dt2[outliers, c("dateOfBirthIsOutlier")] <- "yes"
    dt2$dateOfBirthIsOutlier <- as.factor(dt2$dateOfBirthIsOutlier)
    
    # deliveryDate/Time
    ## IMPORTANT: Do this first
    dt2$deliveryDateMissing <- as.factor(ifelse(is.na(dt2$deliveryDate), "yes", "no"))
    
    outliers <- !is.na(dt2$deliveryDate) & (dt2$deliveryDate - dt2$orderDate) < 0
    dt2[outliers, c("deliveryDate")] <- NA
    dt2$deliveryDateIsOutlier <- "no"
    dt2[outliers, c("deliveryDateIsOutlier")] <- "yes"
    dt2$deliveryDateIsOutlier <- as.factor(dt2$deliveryDateIsOutlier)
    
    # creationDate
    # None is missing, they are all outliers
    #dt2$creationDateMissing <- as.factor(ifelse(is.na(dt2$creationDate), "yes", "no"))
    
    outliers <- dt2$creationDate == as.Date("2011-02-16")
    dt2[outliers, c("creationDate")] <- NA
    dt2$creationDateIsOutlier <- "no"
    dt2[outliers, c("creationDateIsOutlier")] <- "yes"
    dt2$creationDateIsOutlier <- as.factor(dt2$creationDateIsOutlier)
    
    dt2
}

message("Removing outliers in Train and Test Set")
dt.train <- rm.outliers(dt.train)
dt.test <- rm.outliers(dt.test)

dt.train$color <- as.character(dt.train$color)
dt.train[is.na(dt.train$color), c("color")] <- "MISSING"
dt.train$color <- as.factor(dt.train$color)

dt.test$color <- as.character(dt.test$color)
dt.test[is.na(dt.test$color), c("color")] <- "MISSING"
dt.test$color <- as.factor(dt.test$color)

rename <- function(dt2) {
    ## price -> itemPrice
    dt2$itemPrice <- dt2$price
    dt2$price <- NULL
    
    ## color -> itemColor
    dt2$itemColor <- dt2$color
    dt2$color <- NULL
    
    ## size -> itemSize
    dt2$itemSize <- droplevels(as.factor(toupper(dt2$size)))
    dt2$size <- NULL
    
    ## Delivery Date
    dt2$orderDeliveryDate <- dt2$deliveryDate
    dt2$deliveryDate <- NULL
    
    ## State
    dt2$customerState <- dt2$state
    dt2$state <- NULL
    dt2
}

dt.train <- rename(dt.train)
dt.test <- rename(dt.test)

message("Adding Features to the Train and Test Set")
dt.train <- add.features(dt.train)
dt.test <- add.features(dt.test)

if (schatti) {
    dt.train <- add.features.schatti(dt.train)
    dt.test <- add.features.schatti(dt.test)
}

dt.dmc.ids <- list(test=list(), train=list())
if (length(args) > 0) {
    build <- unlist(args)
} else {
    build <- c("M3", "M30", "M31")
}
for (i in build) {
    dt.dmc.ids$train[[i]] <- as.numeric(as.character(read.csv(paste("eva/", i, "_train.txt", sep=""))$orderItemID))
    dt.dmc.ids$test[[i]] <- as.numeric(as.character(read.csv(paste("eva/", i, "_test.txt", sep=""))$orderItemID))
}


c1 <- unique(union(dt.dmc.ids$train$T1, dt.dmc.ids$test$T1))
c2 <- unique(union(dt.dmc.ids$train$T2, dt.dmc.ids$test$T2))
c3 <- unique(union(dt.dmc.ids$train$T2, dt.dmc.ids$test$T3))

c1.itemids <- unique(dt.train[c1, ]$itemID)
c2.itemids <- unique(dt.train[c2, ]$itemID)
c3.itemids <- unique(dt.train[c3, ]$itemID)

dt.merged <- rbind(dt.train[, !names(dt.train) %in% c("returnShipment"), with=F], dt.test)

message("Adding Features that can be computed on ALL data")
dt.train <- add.features.all(dt.train, dt.merged)
dt.test <- add.features.all(dt.test, dt.merged)

if (schatti) {
    dt.train <- add.features.schatti.all(dt.train)
    dt.test <- add.features.schatti.all(dt.test)
}

message("Creating M sets")

dt.dmc <- foreach(i=names(dt.dmc.ids$train)) %dopar% {
    message(paste("Creating Data Set", i))
    train.ids <- dt.dmc.ids$train[[i]]
    test.ids <- dt.dmc.ids$test[[i]]
    list(
        train=add.features.otf(dt.train[train.ids, ], dt.train[-(test.ids), ]),
        test=add.features.otf(dt.train[test.ids, ], dt.train[-(test.ids), ]))
}

dt.test <- add.features.otf(dt.test, dt.train)

names(dt.dmc) <- names(dt.dmc.ids$train)

nas <- function(x) which(is.na(dt.train), T)

na <- list(
    dmc=sapply(dt.dmc, function(x) sapply(x, nas, simplify=F), simplify=F),
    train=nas(dt.train),
    test=nas(dt.test))

message("Serializing Data Frames")
saveRDS(dt.train, file="data.train.RData")
saveRDS(dt.test, file="data.test.RData")
saveRDS(dt.dmc, file="data.dmc.RData")
saveRDS(dt.dmc.ids, file="data.dmc.ids.RData")
#saveRDS(na, file="na.RData")