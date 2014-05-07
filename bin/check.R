#!/usr/bin/env Rscript

source("R/data.R")

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

dt.train.orig <- read.csv("task/orders_train.txt", sep=";",
                     colClasses=dt.classes, na.strings=dt.na.strings)
dt.train.orig$returnShipment <- NULL
dt.train.orig$holiday <- read.csv("task/orders_train.holiday.txt")$holiday
dt.test.orig <- read.csv("task/orders_class.txt", sep=";",
                    colClasses=dt.classes, na.strings=dt.na.strings)
dt.test.orig$holiday <- read.csv("task/orders_class.holiday.txt")$holiday

check <- function(cond, msg) {
    message(paste("Checking:", msg))
    if (!cond) {
        stop("FAILED")
    }
}

check.missing <- function(actual, original, name) {
    check(
        all(actual[[paste(name, "Missing", sep="")]] == as.factor(ifelse(is.na(original[[name]]), "yes", "no"))),
        paste(name, "Missing", sep=""))
}


for (a in c("deliveryDate", "dateOfBirth", "creationDate")) {
    check.missing(dt.train, dt.train.orig, a)
    check.missing(dt.test, dt.test.orig, a)
}


check(all(dt.train$deliveryDateMissing == as.factor(ifelse(is.na(dt.train.orig$deliveryDate), "yes", "no"))),
      "Checking deliveryDateMissing Attribute in the Train Set")

check(all(dt.test$deliveryDateMissing == as.factor(ifelse(is.na(dt.test.orig$deliveryDate), "yes", "no"))),
      "Checking deliveryDateMissing Attribute in the Train Set")

check(all(!(is.na(dt.train$dateOfBirth) &
                dt.train$dateOfBirthMissing == "no" &
                dt.train$dateOfBirthIsOutlier == "no")),
      "Checking if dateOfBirthMissing in the Train Set")

check(all(!(is.na(dt.train$dateOfBirth) &
                dt.train$dateOfBirthMissing == "no" &
                dt.train$dateOfBirthIsOutlier == "no")),
      "Checking if dateOfBirthMissing in the Test Set")

for (s in names(dt.dmc)) {
    message(paste("Checking Set", s))
    oid.train <- as.numeric(as.character(read.csv(paste("eva/", s, "_train.txt", sep=""))$orderItemID))
    oid.test <- as.numeric(as.character(read.csv(paste("eva/", s, "_test.txt", sep=""))$orderItemID))
        
    check(all(dt.dmc[[s]]$train$orderItemID == oid.train), "Train Set: Matching OrderItemID")
    check(all(dt.dmc[[s]]$test$orderItemID == oid.test), "Test Set: Matching OrderItemID")
    
}

