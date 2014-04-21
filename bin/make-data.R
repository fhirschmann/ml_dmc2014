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

feat.simple <- function(dt) {
    dt2 <- data.table(dt)
    
    #dt2$creationDateMissing <- as.factor(ifelse(is.na(dt2$creationDate), "yes", "no"))
    
    dt2$deliveryTime <- as.integer(dt2$deliveryDate - dt2$orderDate)
    dt2$deliveryDateMissing <- as.factor(ifelse(is.na(dt2$deliveryDate), "yes", "no"))
    dt2$orderWeekday <- as.ordered(as.factor(lubridate::wday(dt2$orderDate, label=T, abbr=F)))
    
    # Customer age in Years
    dt2$customerAge <- as.integer(year(dt2$orderDate) - year(dt2$dateOfBirth))
    dt2$dateOfBirthMissing <- as.factor(ifelse(is.na(dt2$dateOfBirth), "yes", "no"))
    
    # Account age in Days
    dt2$accountAge <- as.numeric(dt2$orderDate - dt2$creationDate)
    
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
    
    dt2
}

dt.train <- feat.simple(dt.train)
dt.test <- feat.simple(dt.test)

save(dt.train, dt.test, file="data.RData")
