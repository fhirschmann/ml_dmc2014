# Data Preprocessing goes here

suppressPackageStartupMessages(library(caret))
library(lubridate)

source("R/utils.R")

pp <- function(dt) {
    # Preprocesses DMC2010 data, i.e. cleaning, creating features, etc...
    #
    # Args:
    #   dt: A data frame
    #
    # Returns:
    #   A preprocessed data frame
    
    ## Binary Predictors
    dt_binary <- c("voucher", "title", "newsletter", "gift", "shippingcosts")
    dt[dt_binary] <- lapply(dt[dt_binary], as.binary)
    if ("target90" %in% names(dt)) dt$target90 <- as.binary(dt$target90)
    
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
    
    # Add some features
    dt$deliverydatediff <- dt$deliverydatepromised - dt$deliverydatereal
    dt$month <- as.factor(month(dt$date))
    dt$weekday <- as.factor(wday(dt$date))
    
    dt
}

# Lazy evaluation: dt and dt.test get constructed when needed
delayedAssign("dt", pp(read.csv("task2010/dmc2010_train.txt", sep=";")))
delayedAssign("dt.test", pp(read.csv("task2010/dmc2010_class.txt", sep=";")))
