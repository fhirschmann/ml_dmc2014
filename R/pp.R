# Data Preprocessing goes here

suppressPackageStartupMessages(library(caret))

source("R/utils.R")

# Read in some data
dt <- read.csv("task2010/dmc2010_train.txt", sep=";")

## Remove Zero-Variance Predictors
dt$points <- NULL
dt <- droplevels(dt)

## Binary Predictors
dt_binary <- c("voucher", "title", "newsletter", "gift", "shippingcosts", "target90")
dt[dt_binary] <- lapply(dt[dt_binary], as.binary)

## Nominal Predictors (factors)
dt_factors <- c("customernumber", "salutation",
                "domain", "model", "paymenttype", "deliverytype",
                "invoicepostcode", "delivpostcode",
                "advertisingdatacode", "case", "numberitems",
                "entry")
dt[dt_factors] <- lapply(dt[dt_factors], as.factor)

## Date Predictors
dt_dates <- c("date", "datecreated", "deliverydatepromised", "deliverydatereal")
dt[dt_dates] <- lapply(dt[dt_dates], as.Date)

# Set empty strings to NA
dt[dt$delivpostcode == "", ]$delivpostcode <- NA
dt[dt$advertisingdatacode == "", ]$advertisingdatacode <- NA

# Work on 10% of the original data
set.seed(42)
dt2 <- dt[createDataPartition(dt$voucher, p=0.1, list=FALSE),]

# Add some features
dt2$deliverydatediff <- dt2$deliverydatepromised - dt2$deliverydatereal