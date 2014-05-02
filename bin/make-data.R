#!/usr/bin/env Rscript
library(data.table)
library(plyr)
library(lubridate)
library(vcd)
library(zoo)

source("R/feat.R")

# Read in the Data
dt.na.strings <- c("NA", "", "??", "?")

dt.classes <- c(
    "orderItemID"="factor",
    "orderDate"="Date",
    "deliveryDate"="Date",
    "dateOfBirth"="Date",
    "itemID"="factor",
    "size"="factor",
    "manufacturerID"="factor",
    "customerID"="factor",
    "creationDate"="Date"
)

dt.train <- read.csv("task/orders_train.txt", sep=";",
                     colClasses=dt.classes, na.strings=dt.na.strings)
dt.train$returnShipment <- revalue(as.factor(dt.train$returnShipment), c("0"="no", "1"="yes"))
dt.train$holiday <- read.csv("task/orders_train.holiday.txt")$holiday
dt.test <- read.csv("task/orders_class.txt", sep=";",
                    colClasses=dt.classes, na.strings=dt.na.strings)
#dt.test$holiday <- read.csv("task/orders_class.holiday.txt")$holiday

#ideen für features:
#größe des customers aus bestellen items ermitteln
#preisfeature: abweichung von preis (höher, niedriger, gleich)
#anrede: runterbrechen auf "männlich" / "weiblich" (falls kein großer unterschied bei returnshipments zwischen anreden)

dt.train$returnShipment <- revalue(dt.train$returnShipment, c("0"="no", "1"="yes"))

dt.train <- add.features(dt.train)
dt.test <- add.features(dt.test)

rm.outliers <- function(dt) {
    dt2 <- dt
    
    # dateOfBirth/Age
    outliers <- with(dt2,
                     !is.na(dateOfBirth) 
                     & (dateOfBirth == as.Date("1949-11-19")
                        | customerAge > 85
                        | customerAge < 19))
    dt2[outliers, ]$dateOfBirth <- NA
    dt2[outliers, ]$customerAge <- NA
    dt2$dateOfBirthIsOutlier <- "no"
    dt2[outliers, ]$dateOfBirthIsOutlier <- "yes"
    dt2$dateOfBirthIsOutlier <- as.factor(dt2$dateOfBirthIsOutlier)
    
    # deliveryDate/Time
    outliers <- !is.na(dt2$deliveryTime) & dt2$deliveryTime < 0
    dt2[outliers, ]$deliveryTime <- NA
    dt2[outliers, ]$deliveryDate <- NA
    dt2$deliveryDateIsOutlier <- "no"
    dt2[outliers, ]$deliveryDateIsOutlier <- "yes"
    dt2$deliveryDateIsOutlier <- as.factor(dt2$deliveryDateIsOutlier)
    
    # creationDate
    outliers <- dt2$creationDate == as.Date("2011-02-16")
    dt2[outliers, ]$creationDate <- NA
    dt2[outliers, ]$accountAge <- NA
    dt2$creationDateIsOutlier <- "no"
    dt2[outliers, ]$creationDateIsOutlier <- "yes"
    dt2$creationDateIsOutlier <- as.factor(dt2$creationDateIsOutlier)
    
    dt2
}

dt.train <- rm.outliers(dt.train)
dt.test <- rm.outliers(dt.test)

fix.missing <- function(dt) {
    dt2 <- dt
    
    nas <- is.na(dt2$color)
    dt2[nas, ]$color <- "other"
    dt2[nas, ]$fewcolors <- "other"
    
    dt2
}

#dt.train <- fix.missing(dt.train)
#dt.test <- fix.missing(dt.test)

dt.dmc.ids <- list(test=list(), train=list())
for (i in c("T1", "T2", "T3", "X1", "X2", "X3")) {
    dt.dmc.ids$train[[i]] <- read.csv(paste("eva/", i, "_train.txt", sep=""))$orderItemID
    dt.dmc.ids$test[[i]] <- read.csv(paste("eva/", i, "_test.txt", sep=""))$orderItemID
}

dt.dmc.ids$train$C <- as.integer(as.character(dt.train[dt.train$orderDate < as.Date("2013-03-01"), ]$orderItemID))
dt.dmc.ids$test$C <- as.integer(as.character(dt.train[dt.train$orderDate >= as.Date("2013-03-01"), ]$orderItemID))


c1 <- unique(union(dt.dmc.ids$train$T1, dt.dmc.ids$test$T1))
c2 <- unique(union(dt.dmc.ids$train$T2, dt.dmc.ids$test$T2))
c3 <- unique(union(dt.dmc.ids$train$T2, dt.dmc.ids$test$T3))

add.collection <- function(dt) {
    dt$collection <- 0
    dt[dt$orderItemID %in% c1, ]$collection <- 1
    dt[dt$orderItemID %in% c2, ]$collection <- 2
    dt[dt$orderItemID %in% c3, ]$collection <- 3
    
    dt$collection <- as.factor(dt$collection)
    dt
}

dt.train <- add.collection(dt.train)
dt.test <- add.collection(dt.test)

dt.dmc <- list()
for (i in names(dt.dmc.ids$train)) {
    train.ids <- dt.dmc.ids$train[[i]]
    test.ids <- dt.dmc.ids$test[[i]]
    dt.dmc[[i]] <- list(
        train=add.features.otf(dt.train[train.ids, ], dt.train[-(test.ids), ]),
        test=add.features.otf(dt.train[test.ids, ], dt.train[-(test.ids), ]))
}

save(dt.train, dt.test, dt.dmc, dt.dmc.ids, file="data.RData")
