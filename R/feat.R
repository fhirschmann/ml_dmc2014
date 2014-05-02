source("R/colormap.R")

add.features <- function(dt) {
    #
    # Place features that can be computed over the entire data set here.
    #
    require(plyr)
    
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
    dt2[, sameItemsOrdered := .N, by=c("itemID", "customerID", "orderDate")]
    
    # Total number of items ordered
    dt2[, customerNumItemsOrdered := .N, by=c("itemID", "customerID", "orderDate")]
    
    # Total number of orders
    dt2[, customerNumOrders := .N, by=c("customerID", "orderDate")]
    
    # Date of first order (per customer)
    dt2[, firstOrderDate := min(orderDate), by=c("customerID")]
    dt2$firstOrderDate <- as.Date(dt2$firstOrderDate)
    
    # Volume of order
    dt2[, orderVolume := sum(price), by=c("customerID", "orderDate")]
    
    # Total volume of customer's order
    dt2[, totalOrderVolume := sum(price), by=c("customerID")]
    
    # Summarize colors
    dt2$fewcolors <- revalue(dt2$color, colormap)
    
    # Fix sizes
    dt2$size <- droplevels(as.factor(toupper(dt2$size)))
    
    # Discretized price
    dt2$discretizedPrice <- cut(dt2$price, c(0, 1:20 * 10, Inf), left=T, right=F)
    
    # Item Discount
    dt2[, itemDiscount := 1 - price / max(price), by=c("itemID", "size")]
    dt2[is.na(dt2$itemDiscount), ]$itemDiscount <- 0
    
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

lsmooth <- function(x, N, alpha=1, mean=0.52) {
    ## laplacian smoothing with known mean
    ##
    ## Args:
    ##   x: count of observations with class i
    ##   N: count of all observations
    ##   alpha: smoothing parameter
    ##   mean: mean of class i observations
    (x + alpha) / (N + (alpha / mean))
}

add.features.otf <- function(to, from) {
    #
    # Place features that should be computed on the fly on the train set only here.
    #
    # Args:
    #   to: data frame to add the features to
    #   from: data frame to calculate the features from
    require(data.table)
    require(plyr)
    
    dt.to <- data.table(to)
    dt.from <- data.table(from[from$deliveryDateMissing == "no", ])
    
    dt.from[, customerReturnRate := lsmooth(sum(returnShipment == "yes"), .N), by=c("customerID")]
    customerRetRate <- unique(dt.from[, c("customerID", "customerReturnRate"), with=F])
    dt.to <- join(dt.to, customerRetRate, by="customerID")
    
    # TODO: Maybe we should group this by c("itemID", "color", "size")
    dt.from[, itemReturnRate := lsmooth(sum(returnShipment == "yes"), .N), by=c("itemID")]
    itemRetRate <- unique(dt.from[, c("itemID", "itemReturnRate"), with=F])
    dt.to <- join(dt.to, itemRetRate, by="itemID")
    
    dt.to
}
