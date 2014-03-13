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
    
    dt2 <- dt
    
    ## Binary Predictors
    dt2_binary <- c("voucher", "title", "newsletter", "gift", "shippingcosts")
    if ("target90" %in% names(dt2)) dt2_binary <- c(dt2_binary, "target90")
    dt2[dt2_binary] <- lapply(dt2[dt2_binary],
                            function(x) revalue(as.factor(x), c("1"="yes", "0"="no")))

    ## Nominal Predictors    
    # Use labels instead of numeric values
    dt2_factors <- list(
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
    
    ## Ordered Predictors
    dt2_ordinal <- c("case")
    dt2[dt2_ordinal] <- lapply(dt2[dt2_ordinal], as.ordered)
    
    for (name in names(dt2_factors)) {
        dt2[[name]] <- as.factor(dt2[[name]])
        if (!is.null(dt2_factors[[name]]))
            dt2[[name]] <- revalue(dt2[[name]], dt2_factors[[name]])
    }
        
    ## Date Predictors
    dt2_dates <- c("date", "datecreated", "deliverydatepromised", "deliverydatereal")
    dt2[dt2_dates] <- lapply(dt2[dt2_dates], as.Date)

    ## Fixes
    # Because delivpostcode has strings in it (EN, Nl, NW) it gets read as factor.
    # invoicepostcode has no strings it it, so it gets read as numeric. However,
    # this causes the leading 0s from invoicepostcode to be removed, which is problematic
    # later when we impute missing values in delivpostcode with invoicepostcode. So
    # we add leading zeros in invoicepostcode
    dt2$invoicepostcode <- as.factor(formatC(as.numeric(dt2$invoicepostcode),
                                             width=2, format="d", flag="0"))

    #dt2$invoiceeqdeliv <- ifelse(as.character(dt$invoicepostcode) == 
    #                                 as.character(dt$delivpostcode) |
    #                                 is.na(dt$delivpostcode), "yes", "no")
    
    # Add some features
    dt2$deliverydatediff <- as.numeric(dt2$deliverydatepromised - dt2$deliverydatereal)
    dt2$month <- as.factor(month(dt2$date))
    dt2$weekday <- as.factor(wday(dt2$date, label=T, abbr=F))

    dt2$instantorder <- as.factor(ifelse(dt2$datecreated == dt2$date, "yes", "no"))
        
    dt2
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
    
    ## Dates: See the slides for details
    t <- 250
    move <- dt2$deliverydatediff > t & !is.na(dt2$deliverydatediff)
    dt2[move,]$deliverydatepromised <- dt2[move,]$deliverydatepromised - years(1)

    dt2$deliverydatediff <- as.numeric(dt2$deliverydatepromised - dt2$deliverydatereal)

    ## wX: Slides, again
    if (any(dt$w0 > 60))
        dt2[dt2$w0 > 60,]$w0 <- mean(dt2[dt2$w0 < 60,]$w0)
    if (any(dt$w1 > 60))
    dt2[dt2$w1 > 60,]$w1 <- mean(dt2[dt2$w1 < 60,]$w1)
    if (any(dt$w3) > 60)
        dt2[dt2$w3 > 60,]$w3 <- mean(dt2[dt2$w3 < 60,]$w3)
    
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

    # Impute missing values for delivpostcode with invoicepostcode
    nas <- is.na(dt2$delivpostcode)
    levels(dt2$delivpostcode) <- c(levels(dt2$delivpostcode),
                                   levels(dt2$invoicepostcode))
    dt2[nas,]$delivpostcode <- dt2[nas,]$invoicepostcode
    
    # Since this is just a warm-up, we'll use the median to impute the deliverydatediff
    dt2[is.na(dt2$deliverydatediff),]$deliverydatediff <- round(median(dt2$deliverydatediff, na.rm=T), 0)

    # And set the deliverydatereal attribute accordingly
    nas <- is.na(dt2$deliverydatereal)
    dt2[nas,]$deliverydatereal <- dt2[nas,]$deliverydatepromised + days(dt2[nas,]$deliverydatediff)

    dt2
}