#!/usr/bin/env Rscript
library(data.table)
library(plyr)
library(lubridate)
library(vcd)
library(zoo)

source("R/colormap.R")

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
dt.test <- read.csv("task/orders_class.txt", sep=";",
                    colClasses=dt.classes, na.strings=dt.na.strings)

# Feature Engineering

#ideen für features:
#größe des customers aus bestellen items ermitteln
#preisfeature: abweichung von preis (höher, niedriger, gleich)
#anrede: runterbrechen auf "männlich" / "weiblich" (falls kein großer unterschied bei returnshipments zwischen anreden)

dt.train$returnShipment <- revalue(dt.train$returnShipment, c("0"="no", "1"="yes"))

add.features <- function(dt) {
    dt2 <- data.table(dt)
    
    #dt2$creationDateMissing <- as.factor(ifelse(is.na(dt2$creationDate), "yes", "no"))
    
    dt2$deliveryTime <- as.integer(dt2$deliveryDate - dt2$orderDate)
    dt2$deliveryDateMissing <- as.factor(ifelse(is.na(dt2$deliveryDate), "yes", "no"))
    dt2$orderWeekday <- as.ordered(as.factor(lubridate::wday(dt2$orderDate, label=T, abbr=F)))
    
    # Customer age in Years
    dt2$customerAge <- as.integer(year(dt2$orderDate) - year(dt2$dateOfBirth))
    dt2$dateOfBirthMissing <- as.factor(ifelse(is.na(dt2$dateOfBirth), "yes", "no"))
    
    # Account age in Days
    dt2$accountAge <- as.integer(dt2$orderDate - dt2$creationDate)
    
    # Number of items ordered with the same ID
    dt2 <- dt2[, sameItemsOrdered := .N, by=c("itemID", "customerID", "orderDate")]
    
    # Date of first order (per customer)
    dt2 <- dt2[, firstOrderDate := min(orderDate), by=c("customerID")]
    dt2$firstOrderDate <- as.Date(dt2$firstOrderDate)
    
    # Volume of order
    dt2 <- dt2[, orderVolume := sum(price), by=c("customerID", "orderDate")]
    
    # Total volume of customer's order
    dt2 <- dt2[, totalOrderVolume := sum(price), by=c("customerID")]
    
    # Summarize colors:
    dt2$fewcolors <- revalue(dt2$color, colormap)
    
    # Fix sizes
    dt2$size <- droplevels(as.factor(toupper(dt2$size)))
    
    # Discretized price
    dt2$discretizedPrice <- cut(dt2$price, c(0, 1:20 * 10, Inf), left=T, right=F)
    
    # West/East Germany
    dt2$westGermany <- revalue(dt2$state, c(
        "Baden-Wuerttemberg"="yes",
        "Bavaria"="yes",
        "Berlin"="no",
        "Brandenburg"="no",
        "Bremen"="yes",
        "Hesse"="yes",
        "Hamburg"="yes",
        "Lower Saxony"="yes",
        "Mecklenburg-Western Pomerania"="no",
        "North Rhine-Westphalia"="yes",
        "Rhineland-Palatinate"="yes",
        "Schleswig-Holstein"="yes",
        "Saarland"="yes",
        "Saxony"="no",
        "Saxony-Anhalt"="no",
        "Thuringia"="no"
    ))
    
    dt2
}

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

dt.train <- fix.missing(dt.train)
dt.test <- fix.missing(dt.test)

save(dt.train, dt.test, file="data.RData")
