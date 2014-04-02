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
        "returnShipment"=c("0"="no", "1"="yes")
    )

    for (name in names(dt2_factors)) {
        if (name %in% colnames(dt2)) {
            dt2[[name]] <- as.factor(dt2[[name]])
            if (!is.null(dt2_factors[[name]]))
                dt2[[name]] <- revalue(dt2[[name]], dt2_factors[[name]])
        }
    }
    
    dt2$size <- as.factor(toupper(dt2$size))
    
        
    ## Date Predictors
    dt2_dates <- c("dateOfBirth", "creationDate", "orderDate", "deliveryDate")
    dt2[dt2_dates] <- lapply(dt2[dt2_dates], as.Date)
    
    dt2$deliveryTime <- as.numeric(dt2$deliveryDate - dt2$orderDate)

    dt2$instantorder <- as.factor(ifelse(dt2$orderDate == dt2$creationDate, "yes", "no"))
    
    # Add some features
    dt2$orderMonth <- as.ordered(as.factor(month(dt2$orderDate)))
    dt2$orderWeekday <- as.ordered(as.factor(wday(dt2$orderDate, label=T, abbr=F)))

    dt2$customerAge <- as.numeric(2013 - year(dt2$dateOfBirth))
    
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

    # Color: set to "other"
    levels(dt2$color) <- c(levels(dt2$color), "other")
    if (any(is.na(dt2$color)))  # test set doesn't have NAs
        dt2[is.na(dt2$color), ]$color <- "other"
    
    # Deliverytime: mean
    dt2[is.na(dt2$deliveryTime), ]$deliveryTime <- round(mean(dt2$deliveryTime, na.rm=T), 0)

    # customerAge: mean
    dt2[is.na(dt2$customerAge), ]$customerAge <- round(mean(dt2$customerAge, na.rm=T), 0)
    
    dt2
}