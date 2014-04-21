
cl <- function(dt) {
    # Cleans DMC2010 data, i.e. outlier removal
    #
    # Args:
    #   dt: A data frame
    #
    # Returns:
    #   A preprocessed data frame
    require(plyr)
    
    dt2 <- dt

    # Outlier Removal
    
    ## deliveryDate/Time
    outliers <- !is.na(dt2$deliveryTime) & dt2$deliveryTime < 0
    dt2[outliers, ]$deliveryTime <- NA
    dt2[outliers, ]$deliveryDate <- NA
    
    ## dateOfBirth/Age
    outliers <- with(dt2,
                     !is.na(dateOfBirth) 
                     & (dateOfBirth == as.Date("1949-11-19")
                        | customerAge > 85
                        | customerAge < 19))

    dt2[outliers, ]$dateOfBirth <- NA
    dt2[outliers, ]$customerAge <- NA

    ## creationDate
    outliers <- with(dt2, creationDate == as.Date("2011-02-16"))
    dt2[outliers, ]$creationDate <- NA
    
    # Discretization
    
    ## TODO
    
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
    dt2[is.na(dt2$deliveryTime), ]$deliveryTime <- as.integer(mean(dt2$deliveryTime, na.rm=T))

    # customerAge: mean
    dt2[is.na(dt2$customerAge), ]$customerAge <- as.integer(mean(dt2$customerAge, na.rm=T))
    
    dt2
}