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
    
    ## Nominal Predictors
    dt_factors <- c("customernumber", "salutation",
                    "domain", "model", "paymenttype", "deliverytype",
                    "invoicepostcode", "delivpostcode",
                    "advertisingdatacode",
                    "entry")
    dt[dt_factors] <- lapply(dt[dt_factors], as.factor)

    ## Ordered Predictors
    dt_ordinal <- c("case")
    dt[dt_ordinal] <- lapply(dt[dt_ordinal], as.ordered)

    # Use labels instead of numeric values
    levels(dt$salutation) <- c("Ms.", "Mr.", "Company")
    levels(dt$paymenttype) <- c("invoice", "cash", "transfer", "transfer_cc")
    levels(dt$domain) <- c("aol.com", "arcor.de", "freenet.de", "gmail.com", "gmx.de",
                           "hotmail.de", "online.de", "onlinehome.de", "t-online.de",
                           "web.de", "yahoo.com", "yahoo.de", "other")
    levels(dt$deliverytype) <- c("dispatch", "collection")
    levels(dt$entry) <- c("shop", "partner")
        
    ## Date Predictors
    dt_dates <- c("date", "datecreated", "deliverydatepromised", "deliverydatereal")
    dt[dt_dates] <- lapply(dt[dt_dates], as.Date)
    
    # Set empty strings to NA
    dt[dt$delivpostcode == "", ]$delivpostcode <- NA
    dt[dt$advertisingdatacode == "", ]$advertisingdatacode <- NA
    
    # Add some features
    dt$deliverydatediff <- as.numeric(dt$deliverydatepromised - dt$deliverydatereal)
    dt$month <- as.factor(month(dt$date))
    dt$weekday <- as.factor(wday(dt$date, label=T, abbr=F))
    
    dt
}

cl <- function(dt) {
    # Cleans DMC2010 data, i.e. outlier removal
    #
    # Args:
    #   dt: A data frame
    #
    # Returns:
    #   A preprocessed data frame
    dt2 <- dt
    dt2[dt2$deliverydatepromised > as.Date("2011-01-01"),]$deliverydatepromised <- NA

    dt2
}