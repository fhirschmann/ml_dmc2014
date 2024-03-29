source("R/colormap.R")

add.features <- function(dt) {
    #
    # Place features that can be computed over the entire data set here.
    #
    require(plyr)
    require(Hmisc)
    dt2 <- data.table(dt)
    
    # Order: Attributes specific to an order (possibly grouped by customerID, orderDate)
    
    ## Delivery Time
    dt2$orderDeliveryTime <- as.integer(dt2$orderDeliveryDate - dt2$orderDate)
    
    ## Weekday of the Order
    dt2$orderWeekday <- as.ordered(as.factor(lubridate::wday(dt2$orderDate, label=T, abbr=F)))
    
    ## Instant
    dt2$orderInstant <- as.factor(ifelse(dt2$creationDate == dt2$orderDate, "yes", "no"))
    
    ## Sum of all prices in the Order
    dt2[, orderSumPrices := sum(itemPrice), by=c("customerID", "orderDate")]
    
    ## Numer of Items in the Order
    dt2[, orderNumItems := length(orderItemID), by=c("customerID", "orderDate")]
        
    ## Number of items ordered with the same ID
    dt2[, orderItemNumSameItemsOrdered := .N, by=c("itemID", "customerID", "orderDate")]
    dt2$orderItemNumSameItemsOrderedGreater1 <- as.factor(ifelse(dt2$orderItemNumSameItemsOrdered > 1, "yes", "no"))
    
    dt2[, orderItemNumSameItemsOrderedColor := .N, by=c("itemID", "customerID", "orderDate", "itemColor")]
    dt2[, orderItemNumSameItemsOrderedSize := .N, by=c("itemID", "customerID", "orderDate", "itemSize")]
    
    # Total number of distinct items ordered
    dt2[, orderNumDistinctItems := length(unique(itemID)), by=c("customerID", "orderDate")]
    dt2$orderNumDistinctItemsGreater1 <- as.factor(ifelse(dt2$orderNumDistinctItems > 1, "yes", "no"))
    
    # Sum of the price
    dt2[, orderSumPrice := sum(itemPrice), by=c("customerID", "orderDate")]
    
    # Item: Attributes specific to an item
    
    ## Summarized colors
    dt2$itemBaseColor <- revalue(dt2$itemColor, colormap)
    
    # Discretized price
    dt2$itemPriceDiscrete <- cut(dt2$itemPrice, c(0, 1:20 * 10, Inf), left=T, right=F)
    
    # Price Levels
    dt2[, itemPriceLevels := length(unique(itemPrice)), by=c("itemID")]
    dt2$itemPriceLevelsGreater1 <- as.factor(ifelse(dt2$itemPriceLevels > 1, "yes", "no"))
    
    # Price Range
    dt2[, itemPriceRange := 1 - (max(itemPrice) - min(itemPrice)) / (max(itemPrice) + 1), by=c("itemID")]
    
    # Item Discount
    dt2[, itemDiscount := 1 - itemPrice / max(itemPrice), by=c("itemID", "itemSize")]
    dt2[is.na(dt2$itemDiscount), c("itemDiscount")] <- 0
    
#     # Item Size Type
#     dt2$itemSizeIsAllDigits <- "no"
#     dt2[all.digits(as.character(dt2$itemSize)), c("itemSizeIsAllDigits")] <- "yes"
#     dt2$itemSizeIsAllDigits <- as.factor(dt2$itemSizeIsAllDigits)
#     
#     dt2$itemSizeIsOneDigit <- dt2$itemSizeIsAllDigits
#     dt2[dt2$itemSizeIsAllDigits == "yes" & nchar(as.character(dt2$itemSize)) != 1, c("itemSizeIsOneDigit")] <- "no"
#     
#     dt2$itemSizeIsTwoDigits <- dt2$itemSizeIsAllDigits
#     dt2[dt2$itemSizeIsAllDigits == "yes" & nchar(as.character(dt2$itemSize)) != 2, c("itemSizeIsTwoDigits")] <- "no"
#     
#     dt2$itemSizeIsFourDigits <- dt2$itemSizeIsAllDigits
#     dt2[dt2$itemSizeIsFourDigits == "yes" & nchar(as.character(dt2$itemSize)) != 4, c("itemSizeIsFourDigits")] <- "no"
#     
#     dt2$itemSizeIsStandardSize <- "no"
#     dt2[as.character(dt2$itemSize) %in% c("M", "S", "XL", "XS", "XXL", "XXXL", "L"), c("itemSizeIsStandardSize")] <- "yes"
#     dt2$itemSizeIsStandardSize <- as.factor(dt2$itemSizeIsStandardSize)

    # Item Collection
    dt2[dt2$itemID < 1478, c("itemCollection")] <- 1
    dt2[dt2$itemID >= 1478 & itemID < 2358, c("itemCollection")] <- 2
    dt2[dt2$itemID >= 2358, c("itemCollection")] <- 3
    dt2$itemCollection <- as.factor(dt2$itemCollection)
    
    # Customer: Attributes specific to a customer
    
    ## Customer Age in years (at order time)
    dt2$customerAgeAtOrderTime <- as.numeric((dt2$orderDate - dt2$dateOfBirth) / 365)
    dt2$customerAgeAtOrderTimeDiscrete <- cut(dt2$customerAgeAtOrderTime, c(10 + 1:14 * 5, Inf))
    
    ## Account age in Years (at order time)
    dt2$customerAccountAgeAtOrderTime <- as.numeric((as.Date("2013-04-30") - dt2$creationDate) / 365)
    dt2$customerAccountAgeAtOrderTimeDiscrete <- cut(dt2$customerAccountAgeAtOrderTime, c(-Inf, 1:10 * 0.2, Inf))
    
    ## Account age in Years (absolute)
    dt2$customerAccountAge <- as.numeric((dt2$orderDate - dt2$creationDate) / 365)
    dt2$customerAccountAgeDiscrete <- cut(dt2$customerAccountAge, c(-Inf, 1:10 * 0.2, Inf))
    
    ## Total number of items ordered
    dt2[, customerNumItemsOrdered := .N, by=c("customerID")]
    
    ## Total number of orders
    dt2[, customerNumOrders := length(unique(orderDate)), by=c("customerID")]
    
    # Date of first order (per customer)
    dt2[, customerFirstOrderDate := min(orderDate), by=c("customerID")]
    dt2$customerFirstOrderDate <- as.Date(dt2$customerFirstOrderDate)
    
    # West/East Germany
    dt2$customerWestGerman <- revalue(dt2$customerState, c(
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
    ##
    ## Place features that should be computed based on one training set
    ## and added to another set here.
    ##
    ## Args:
    ##   to: data frame to add the features to
    ##   from: data frame to calculate the features from
    require(data.table)
    require(plyr)
    
    message(paste("Calculating features for", nrow(to), "instances based on", nrow(from), "instances"))
    
    dt.to <- data.table(to)
    dt.from <- data.table(from[from$deliveryDateMissing == "no", ])
    
    ## total money spent per customer
    dt.from[dt.from$returnShipment == "no", customerMoneySpent := sum(itemPrice), by=c("customerID")]
    customerMoneySpent <- unique(dt.from[dt.from$returnShipment == "no", c("customerID", "customerMoneySpent"), with=F])
    dt.to <- join(dt.to, customerMoneySpent, by="customerID")

    ## customerReturnRate
    dt.from[, customerReturnRate := lsmooth(sum(returnShipment == "yes"), .N), by=c("customerID")]
    customerRetRate <- unique(dt.from[, c("customerID", "customerReturnRate"), with=F])
    dt.to <- join(dt.to, customerRetRate, by="customerID")
    
    # treat unknown customers
    #dt.to[, unknownCustomer := as.factor(ifelse(is.na(customerReturnRate), "yes", "no"))]
    #dt.to[is.na(customerReturnRate), customerReturnRate := 0.52]

    ## itemReturnRate
    # TODO: Maybe we should group this by c("itemID", "color", "size")
    dt.from[, itemReturnRate := lsmooth(sum(returnShipment == "yes"), .N), by=c("itemID", "itemSize")]
    itemRetRate <- unique(dt.from[, c("itemID", "itemReturnRate", "itemSize"), with=F])
    dt.to <- join(dt.to, itemRetRate, by=c("itemID", "itemSize"))
    # treat unknown items
    #dt.to[, unknownItem := as.factor(ifelse(is.na(itemReturnRate), "yes", "no"))]
    #dt.to[is.na(itemReturnRate), itemReturnRate := 0.52]
    
    dt.from[, itemCategoryReturnRate := lsmooth(sum(returnShipment == "yes"), .N), by=c("itemCategory")]
    itemCategoryRetRate <- unique(dt.from[, c("itemCategory", "itemCategoryReturnRate"), with=F])
    dt.to <- join(dt.to, itemCategoryRetRate, by=c("itemCategory"))

    ## colorReturnRate
    dt.from[, itemColorReturnRate := lsmooth(sum(returnShipment == "yes"), .N), by=c("itemColor")]
    cRR <- unique(dt.from[, c("itemColor", "itemColorReturnRate"), with=F])
    dt.to <- join(dt.to, cRR, by=c("itemColor"))
    #dt.to[, unknownColor := as.factor(ifelse(is.na(colorReturnRate), "yes", "no"))]
    #dt.to[is.na(colorReturnRate), colorReturnRate := 0.52]
    
    ## itemColorReturnRate
    dt.from[, itemItemColorReturnRate := lsmooth(sum(returnShipment == "yes"), .N), by=c("itemID","itemBaseColor")]
    itemColorRetRate <- unique(dt.from[, c("itemID","itemBaseColor", "itemItemColorReturnRate"), with=F])
    dt.to <- join(dt.to, itemColorRetRate, by=c("itemID","itemBaseColor"))

    ## baseColorReturnRate
    dt.from[, itemBaseColorReturnRate := lsmooth(sum(returnShipment == "yes"), .N), by=c("itemBaseColor")]
    fcRR <- unique(dt.from[, c("itemBaseColor", "itemBaseColorReturnRate"), with=F])
    dt.to <- join(dt.to, fcRR, by=c("itemBaseColor"))
    #dt.to[, unknownFewColor := as.factor(ifelse(is.na(baseColorReturnRate), "yes", "no"))]
    #dt.to[is.na(baseColorReturnRate), baseColorReturnRate := 0.52]

    ## sizeReturnRate
    dt.from[, itemSizeReturnRate := lsmooth(sum(returnShipment == "yes"), .N), by=c("itemSize")]
    sRR <- unique(dt.from[, c("itemSizeReturnRate", "itemSize"), with=F])
    dt.to <- join(dt.to, sRR, by=c("itemSize"))

    ## manufacturerReturnRate
    dt.from[, manufacturerReturnRate := lsmooth(sum(returnShipment == "yes"), .N), by=c("manufacturerID")]
    mRR <- unique(dt.from[, c("manufacturerID", "manufacturerReturnRate"), with=F])
    dt.to <- join(dt.to, mRR, by="manufacturerID")
    # treat unknown manufacturers
    #dt.to[, unknownManufacturer := as.factor(ifelse(is.na(manufacturerReturnRate), "yes", "no"))]
    #dt.to[is.na(manufacturerReturnRate), manufacturerReturnRate := 0.52]

    dt.to
}

add.features.all <- function(to, from) {
    ## Place features that should be computed over ALL available data and
    ## added to `to`.
    ##
    ## Args:
    ##   to: data frame to add the features to
    ##   from: data frame to calculate the features from
    require(data.table)
    require(plyr)
    
    dt.to <- data.table(to)
    dt.from <- data.table(from[from$deliveryDateMissing == "no", ])
    
    # Next/Prev order (in days)
    orderDates <- unique(dt.from[, c("customerID", "orderDate"), with=F])
    setkeyv(orderDates, c("customerID", "orderDate"))
    orderDates[, index := 1:.N, by=c("customerID")]
    orderDates[, orderDateNext := as.Date(orderDate[index + 1]), by=c("customerID")]
    orderDates[, orderDatePrev := c(NA, head(orderDate, -1)), by=c("customerID")]
    #orderDates[, orderDatePrev := as.Date(orderDate[index - 1]), by=c("customerID")]
    orderDates$orderTimeNext <- as.integer(as.Date(orderDates$orderDateNext) - as.Date(orderDates$orderDate))
    orderDates$orderTimePrev <- as.integer(as.Date(orderDates$orderDatePrev) - as.Date(orderDates$orderDate))
    dt.to <- join(dt.to, orderDates[, c("orderTimeNext", "orderDate", "orderTimePrev", "customerID"), with=F],
                  by=c("customerID", "orderDate"))
    
    # favorite color
    dt.from[, customerFavoriteColor := names(which.max(table(itemColor))), by=c('customerID')]
    dt.from$customerFavoriteColor <- as.factor(dt.from$customerFavoriteColor)
    customerFavoriteColor <- unique(dt.from[, c("customerID", "customerFavoriteColor"), with=F])
    dt.to <- join(dt.to, customerFavoriteColor, by=c("customerID"))
    
    dt.to$customerItemIsFavoriteColor <- as.factor(ifelse(
        as.character(dt.to$customerFavoriteColor) == as.character(dt.to$itemColor), "yes", "no"))
    
    # favorite baseColor
    dt.from[, customerFavoriteBaseColor := names(which.max(table(itemBaseColor))), by=c('customerID')]
    dt.from$customerFavoriteBaseColor <- as.factor(dt.from$customerFavoriteBaseColor)
    
    customerFavoriteBaseColor <- unique(dt.from[, c("customerID", "customerFavoriteBaseColor"), with=F])
    dt.to <- join(dt.to, customerFavoriteBaseColor, by=c("customerID"))
    dt.to$customerItemIsFavoriteBaseColor <- as.factor(ifelse(
        as.character(dt.to$customerFavoriteBaseColor) == as.character(dt.to$itemBaseColor), "yes", "no"))
    
    # favorite size
    dt.from[, customerFavoriteSize := names(which.max(table(itemSize))), by=c('customerID')]
    dt.from$customerFavoriteSize <- as.factor(dt.from$customerFavoriteSize)
    customerFavoriteSize <- unique(dt.from[, c("customerID", "customerFavoriteSize"), with=F])
    dt.to <- join(dt.to, customerFavoriteSize, by=c("customerID"))
    dt.to$customerItemIsFavoriteSize <- as.factor(ifelse(
        as.character(dt.to$customerFavoriteSize) == as.character(dt.to$itemSize), "yes", "no"))
    
    ## customerDiscountRate & tendency Kundenneugung reduzierte Ware zu bestellen
    dt.from[, customerDiscountRate := lsmooth(sum(itemPriceRange > 0 & itemDiscount > 0), .N, mean=0.5), by=c("customerID")]
    cdr <- unique(dt.from[, c("customerID", "customerDiscountRate"), with=F])
    temp<-quantile(cdr$customerDiscountRate)
    #<=25% quantile-> low; >=75 quantile->high; neutral sonst 
    cdr[customerDiscountRate>=temp[4],customerDiscountTendency:= "High"]
    cdr[customerDiscountRate<=temp[2],customerDiscountTendency:= "Low"]
    cdr[is.na(customerDiscountTendency), customerDiscountTendency := "Neutral"]
    cdr$customerDiscountTendency <- as.factor(cdr$customerDiscountTendency)
    cdr[,customerDiscountRate:=NULL]
    dt.to <- join(dt.to, cdr, by=c("customerID"))
    
    dt.to
}
