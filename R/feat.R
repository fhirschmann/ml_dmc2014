source("R/colormap.R")

add.features <- function(dt) {
    #
    # Place features that can be computed over the entire data set here.
    #
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
    dt2[, itemsOrdered := .N, by=c("itemID", "customerID", "orderDate")]
    
    # Total number of orders
    dt[, customerNumOrders := .N, by=("customerID", "orderDate")]
    
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

add.features.otf <- function(dt) {
    #
    # Place features that should be computed on the fly on the train set only here.
    #
    require(data.table)
    dt2 <- data.table(dt)
    
    dt2[, customerReturnRate := sum(returnShipment == "yes") / .N, by=c("customerID")]
    
    dt2
}