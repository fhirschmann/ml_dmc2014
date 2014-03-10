# Data Preprocessing goes here

suppressPackageStartupMessages(library(caret))
library(lubridate)
library(plyr)

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
    if ("target90" %in% names(dt)) dt_binary <- c(dt_binary, "target90")
    dt[dt_binary] <- lapply(dt[dt_binary],
                            function(x) revalue(as.factor(x), c("1"="yes", "0"="no")))

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
    dt_factors <- list(
        "customernumber"=NULL,
        "salutation"=c("0"="Ms.", "1"="Mr.", "2"="Company"),
        "domain"=c("0"="aol.com", "1"="arcor.de", "2"="freenet.de", "3"="gmail.com",
                   "4"="gmx.de", "5"="hotmail.de", "6"="online.de", "7"="onlinehome.de",
                   "8"="t-online.de", "9"="web.de", "10"="yahoo.com", "11"="yahoo.de",
                   "12"="other"),
        "model"=NULL,
        "paymenttype"=c("0"="invoice", "1"="cash", "2"="transfer_current", "3"="transfer_cc"),
        "deliverytype"=c("0"="dispatch", "1"="collection"),
        "invoicepostcode"=NULL,
        "delivpostcode"=NULL,
        "advertisingdatacode"=NULL,
        "entry"=c("0"="shop", "1"="partner")
    )
    
    for (name in names(dt_factors)) {
        dt[[name]] <- as.factor(dt[[name]])
        if (!is.null(dt_factors[[name]]))
            dt[[name]] <- revalue(dt[[name]], dt_factors[[name]])
    }
        
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
    #dt2[dt2$deliverydatepromised > as.Date("2011-01-01"),]$deliverydatepromised <- NA

    # Outlier handling
    ## Incorrect date in deliverydatepromised
    outliers <- dt2$deliverydatepromised > as.Date("2011-01-01")
    year(dt2[outliers,]$deliverydatepromised) <- year(dt[outliers,]$date)
    dt2$deliverydatediff <- as.numeric(dt2$deliverydatepromised - dt2$deliverydatereal)
    
    ## When the deliverydatediff is off by close to 1/2 year, it's very likely that 
    ## deliverydatereal should be subtracted by 1/2 year. Please see doc/notes.Rmd
    
    t <- 340
    
    outliers <- dt2$deliverydatediff < -t & !is.na(dt2$deliverydatediff)
    dt2[outliers,]$deliverydatereal <- dt2[outliers,]$deliverydatereal - years(1)

    outliers <- dt2$deliverydatediff > t & !is.na(dt2$deliverydatediff)
    dt2[outliers,]$deliverydatereal <- dt2[outliers,]$deliverydatereal + years(1)

    dt2$deliverydatediff <- as.numeric(dt2$deliverydatepromised - dt2$deliverydatereal)
    
    # Some are off by two years, so we just do it again
    outliers <- dt2$deliverydatediff < -t & !is.na(dt2$deliverydatediff)
    dt2[outliers,]$deliverydatereal <- dt2[outliers,]$deliverydatereal - years(1)
    
    outliers <- dt2$deliverydatediff > t & !is.na(dt2$deliverydatediff)
    dt2[outliers,]$deliverydatereal <- dt2[outliers,]$deliverydatereal + years(1)
    
    #dt2[dt2$deliverydatediff < -15 | dt2$deliverydatediff > 21 | is.na(dt2$deliverydatediff),]$deliverydatediff <- NA
    dt2$deliverydatediff <- as.numeric(dt2$deliverydatepromised - dt2$deliverydatereal)
    dt2
}

im <- function(dt) {
    # Imputes missing values.
    #
    # Args:
    #   dt: A data frame
    
    dt2 <- dt
    
    # Advertising Data Code: Just use the data code "NA" for missing values, because
    # these values are probably missing because no advertising partner was used when
    # making the purchases.
    levels(dt2$advertisingdatacode) <- c(levels(dt2$advertisingdatacode), "NA")
    dt2[is.na(dt2$advertisingdatacode),]$advertisingdatacode <- "NA"

    # Can't think of a reason to impute missing values for delivpostcode either
    levels(dt2$delivpostcode) <- c(levels(dt2$delivpostcode), "NA")
    dt2[is.na(dt2$delivpostcode),]$delivpostcode <- "NA"
    
    # Since this is just a warm-up, we'll use the median to impute the deliverydatediff
    dt2[is.na(dt2$deliverydatediff),]$deliverydatediff <- round(median(dt2$deliverydatediff, na.rm=T), 0)

    # And set the deliverydatereal attribute accordingly
    nas <- is.na(dt2$deliverydatereal)
    dt2[nas,]$deliverydatereal <- dt2[nas,]$deliverydatepromised + days(dt2[nas,]$deliverydatediff)

    dt2
}