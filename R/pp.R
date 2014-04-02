# Data Preprocessing goes here

suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(plyr))

pp <- function(dt) {
    # Preprocesses DMC2010 data, i.e. cleaning, creating features, etc...
    #
    # Args:
    #   dt: A data frame
    #
    # Returns:
    #   A preprocessed data frame
    
    dt2 <- dt
    message("Preprocessing Data Frame")

    ## Nominal Predictors    
    # Use labels instead of numeric values
    dt2_factors <- list(
        "customerID"=NULL,
        "manufacturerID"=NULL,
        "itemID"=NULL,
        "returnShipment"=NULL
    )

    for (name in names(dt2_factors)) {
        if (name %in% colnames(dt2)) {
            dt2[[name]] <- as.factor(dt2[[name]])
            if (!is.null(dt2_factors[[name]]))
                dt2[[name]] <- revalue(dt2[[name]], dt2_factors[[name]])
        }
    }
    
    ## Ordered Predictors
    #dt2_ordinal <- c("size")
    #dt2[dt2_ordinal] <- lapply(dt2[dt2_ordinal], as.ordered)
        
    ## Date Predictors
    dt2_dates <- c("dateOfBirth", "creationDate", "orderDate", "deliveryDate")
    dt2[dt2_dates] <- lapply(dt2[dt2_dates], as.Date)
    
    dt2$deliveryTime <- as.numeric(dt2$deliveryDate - dt2$orderDate)

    dt2$instantorder <- as.factor(ifelse(dt2$orderDate == dt2$creationDate, "yes", "no"))
    
    # Add some features
    #dt2$month <- as.factor(month(dt2$date))
    #dt2$weekday <- as.factor(wday(dt2$date, label=T, abbr=F))
    
    #dt2$w0all <- Reduce("+", lapply(0:10, function(x) dt2[[paste("w", x, sep="")]]))
    
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
    
    # Size: This needs more fixing!
    dt2$size <- as.factor(toupper(dt2$size))

    # Incorrect Dates:
    outliers <- !is.na(dt2$deliveryTime) & dt2$deliveryTime < 0
    dt2[outliers, ]$deliveryTime <- NA
    dt2[outliers, ]$deliveryDate <- NA
    
    dt2
}

im <- function(dt) {
    # Imputes missing values.
    #
    # Args:
    #   dt: A data frame
    
    dt2 <- dt
    dt2
}