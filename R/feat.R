source("R/colormap.R")

add.features <- function(dt) {
    #
    # Place features that can be computed over the entire data set here.
    #
    require(plyr)
    dt2 <- data.table(dt)
    
    dt2$deliveryTime <- as.integer(dt2$deliveryDate - dt2$orderDate)
    dt2$orderWeekday <- as.ordered(as.factor(lubridate::wday(dt2$orderDate, label=T, abbr=F)))
    
    # Instant Order
    dt2$instantOrder <- as.factor(ifelse(dt2$creationDate == dt2$orderDate, "yes", "no"))
    
    # Customer age in Years (at order time)
    dt2$customerAgeAtOrderTime <- as.numeric((dt2$orderDate - dt2$dateOfBirth) / 365)
    dt2$customerAgeAtOrderTimeDiscrete <- cut(dt2$customerAgeAtOrderTime, c(10 + 1:14 * 5, Inf))
    
    # Account age in Years (at order time)
    dt2$customerAccountAgeAtOrderTime <- as.numeric((as.Date("2013-04-30") - dt2$creationDate) / 365)
    dt2$customerAccountAgeAtOrderTimeDiscrete <- cut(dt2$customerAccountAgeAtOrderTime, c(-Inf, 1:10 * 0.2, Inf))
    
    # Account age in Years (absolute)
    dt2$customerAccountAge <- as.numeric((dt2$orderDate - dt2$creationDate) / 365)
    dt2$customerAccountAgeDiscrete <- cut(dt2$customerAccountAge, c(-Inf, 1:10 * 0.2, Inf))
    
    # Number of items ordered with the same ID
    dt2[, sameItemsOrdered := .N, by=c("itemID", "customerID", "orderDate")]
    dt2$sameItemsOrderedGreater2 <- as.factor(ifelse(dt2$sameItemsOrdered > 2, "yes", "no"))
    
    # Total number of items ordered
    dt2[, customerNumItemsOrdered := .N, by=c("customerID")]
    
    # Total number of distinct items ordered
    dt2[, orderDistinctItems := length(unique(itemID)), by=c("customerID", "orderDate")]
    dt2$orderDistinctItemsGreater1 <- as.factor(ifelse(dt2$orderDistinctItems > 1, "yes", "no"))
    
    # Total number of orders
    dt2[, customerNumOrders := .N, by=c("customerID", "orderDate")]
    
    # Date of first order (per customer)
    dt2[, customerFirstOrderDate := min(orderDate), by=c("customerID")]
    dt2$customerFirstOrderDate <- as.Date(dt2$customerFirstOrderDate)

    # price volume of order
    dt2[, orderVolume := sum(price), by=c("customerID", "orderDate")]

    # size of order
    dt2[, orderSize := length(orderItemID), by=c("customerID", "orderDate")]
    
    # Total volume of customer's order
    dt2[, customerTotalOrderVolume := sum(price), by=c("customerID")]
    
    # Summarize colors
    dt2$baseColor <- revalue(dt2$color, colormap)
    
    # Fix sizes
    dt2$size <- droplevels(as.factor(toupper(dt2$size)))
    
    # Discretized price
    dt2$priceDiscrete <- cut(dt2$price, c(0, 1:20 * 10, Inf), left=T, right=F)
    
    # Price Levels
    dt2[, itemPriceLevels := length(unique(price)), by=c("itemID")]
    dt2$itemPriceLevelsGreater1 <- as.factor(ifelse(dt2$itemPriceLevels > 1, "yes", "no"))
    
    # Price Range
    #dt2[, itemPriceRange := 1 - (max(price) / min(price)) / max(price)]
    
    # Item Discount
    dt2[, itemDiscount := 1 - price / max(price), by=c("itemID", "size")]
    
    dt2[is.na(dt2$itemDiscount), c("itemDiscount")] <- 0
    
    # Item Collection
    dt2[dt2$itemID < 1100, c("itemCollection")] <- 1
    dt2[dt2$itemID >= 1100 & itemID < 2100, c("itemCollection")] <- 2
    dt2[dt2$itemID >= 2100, c("itemCollection")] <- 3
    dt2$itemCollection <- as.factor(dt2$itemCollection)

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
    dt.from[dt.from$returnShipment == "no", customerMoneySpent := sum(price), by=c("customerID")]
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
    dt.from[, itemReturnRate := lsmooth(sum(returnShipment == "yes"), .N), by=c("itemID", "size")]
    itemRetRate <- unique(dt.from[, c("itemID", "itemReturnRate", "size"), with=F])
    dt.to <- join(dt.to, itemRetRate, by=c("itemID", "size"))
    # treat unknown items
    #dt.to[, unknownItem := as.factor(ifelse(is.na(itemReturnRate), "yes", "no"))]
    #dt.to[is.na(itemReturnRate), itemReturnRate := 0.52]

    ## colorReturnRate
    dt.from[, colorReturnRate := lsmooth(sum(returnShipment == "yes"), .N), by=c("color")]
    cRR <- unique(dt.from[, c("color", "colorReturnRate"), with=F])
    dt.to <- join(dt.to, cRR, by=c("color"))
    #dt.to[, unknownColor := as.factor(ifelse(is.na(colorReturnRate), "yes", "no"))]
    #dt.to[is.na(colorReturnRate), colorReturnRate := 0.52]

    ## baseColorReturnRate
    dt.from[, baseColorReturnRate := lsmooth(sum(returnShipment == "yes"), .N), by=c("baseColor")]
    fcRR <- unique(dt.from[, c("baseColor", "baseColorReturnRate"), with=F])
    dt.to <- join(dt.to, fcRR, by=c("baseColor"))
    #dt.to[, unknownFewColor := as.factor(ifelse(is.na(baseColorReturnRate), "yes", "no"))]
    #dt.to[is.na(baseColorReturnRate), baseColorReturnRate := 0.52]

    ## sizeReturnRate
    dt.from[, sizeReturnRate := lsmooth(sum(returnShipment == "yes"), .N), by=c("size")]
    sRR <- unique(dt.from[, c("sizeReturnRate", "size"), with=F])
    dt.to <- join(dt.to, sRR, by=c("size"))

    ## manufacturerReturnRate
    dt.from[, manufacturerReturnRate := lsmooth(sum(returnShipment == "yes"), .N), by=c("manufacturerID")]
    mRR <- unique(dt.from[, c("manufacturerID", "manufacturerReturnRate"), with=F])
    dt.to <- join(dt.to, mRR, by="manufacturerID")
    # treat unknown manufacturers
    dt.to[, unknownManufacturer := as.factor(ifelse(is.na(manufacturerReturnRate), "yes", "no"))]
    dt.to[is.na(manufacturerReturnRate), manufacturerReturnRate := 0.52]

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
    
    # favorite color
    dt.from[, customerFavoriteColor := names(which.max(table(color))), by=c('customerID')]
    dt.from$customerFavoriteColor <- as.factor(dt.from$customerFavoriteColor)
    customerFavoriteColor <- unique(dt.from[, c("customerID", "customerFavoriteColor"), with=F])
    dt.to <- join(dt.to, customerFavoriteColor, by=c("customerID"))
    
    dt.to$customerItemIsFavoriteColor <- as.factor(ifelse(
        as.character(dt.to$customerFavoriteColor) == as.character(dt.to$color), "yes", "no"))
    
    # favorite baseColor
    dt.from[, customerFavoriteBaseColor := names(which.max(table(baseColor))), by=c('customerID')]
    dt.from$customerFavoriteBaseColor <- as.factor(dt.from$customerFavoriteBaseColor)
    
    customerFavoriteBaseColor <- unique(dt.from[, c("customerID", "customerFavoriteBaseColor"), with=F])
    dt.to <- join(dt.to, customerFavoriteBaseColor, by=c("customerID"))
    dt.to$customerItemIsFavoriteBaseColor <- as.factor(ifelse(
        as.character(dt.to$customerFavoriteBaseColor) == as.character(dt.to$baseColor), "yes", "no"))
    
    dt.to
}

#     ###price features##########################################################################
#     temp <- rbind(data.frame(dt.from[,list(itemID,price,orderDate,size,color)], group="train"),
#                   data.frame(dt.to[,list(itemID,price,orderDate,size,color)], group="test"))
#     #1,2 ->min and max price
#     itemPriceRange<- temp[,list(min(price),max(price)),by=list(itemID)]
#     setnames(itemPriceRange,c("V1", "V2"),c("min","max"))
#     #3 -> quantity of price levels
#     setkey(itemPriceRange,itemID)
#     itemPriceRange=itemPriceRange[dt.from[order(itemID,-price,decreasing=FALSE),length(itemID),by=list(itemID,price)][,length(price),by=itemID]]
#     setnames(itemPriceRange,"V1","levels")
#     #4 -> boolean price range TRUE FALSE
#     itemPriceRange[,priceRange:=0] #hinzufügen Feature priceRange
#     itemPriceRange[max!=min,priceRange:=1] #0:keine Range       1:Range
#     #5 -> absolute Price Range
#     itemPriceRange[,AbsPriceRange:=max-min] #hinzufügen Feature abs priceDiff
#     
#     #join itemPriceRange an itemID
#     setkey(dt.from,itemID)
#     dt.from<-dt.from[itemPriceRange]
#     setkey(dt.to,itemID)
#     dt.to<-dt.to[itemPriceRange]
#     #6 -> actual discount on piece
#     dt.from[, actDiscount := round(1-(price/max),digits=2)*100]
#     dt.to[, actDiscount := round(1-(price/max),digits=2)*100]
#     #7 -> boolean discounted TRUE False
#     dt.from$discount <- as.factor(ifelse(dt.from$actDiscount==0, "no", "yes"))
#     dt.to$discount <- as.factor(ifelse(dt.to$actDiscount==0, "no", "yes"))
#     
#     ####customer features##########################################################################
#     #lifetime returnShipment Rate
#     temp<-dt.from[,list(itemID,customerID,returnShipment)]
#     temp<-temp[order(customerID,decreasing=TRUE),list(sum(returnShipment == "yes"),
#                                                       length(itemID)),by=list(customerID)]
#     #1 custReturnRateSchatti - Meine Idee
#     temp[V2 >= median(temp$V2), custReturnRateSchatti:=round((V1/V2),digits=2)*100]
#     temp[V2 < median(temp$V2), custReturnRateSchatti:=round((V1 + temp[,round(sum(V1)/sum(V2),digits=2)]*(median(temp$V2)-V1))/median(temp$V2),digits=2)*100]
#     
#     #     #2 custReturnRateSimon - Michaels Entwurf
#     #     temp[round((V1/V2),digits=2)>=round(sum(temp$V1)/sum(temp$V2),digits=2),
#     #          custReturnRateSimon:=round(V1/(V2+1),digits=2)*100]
#     #     temp[round((V1/V2),digits=2)<round(sum(temp$V1)/sum(temp$V2),digits=2),
#     #          custReturnRateSimon:=round((V1+1)/(V2+1),digits=2)*100]
#     #3,4
#     setnames(temp,c("V1", "V2"),c("custItemsReturned","custItemsOrdered"))
#     #join die temp an customerID
#     setkey(dt.from,customerID)
#     dt.from<-dt.from[temp]
#     setkey(dt.to,customerID)
#     dt.to<-dt.to[temp]
#     
#     ###itemID features##########################################################################
#     #return rate of items - hight rollers setting a fetaure on 1 low rollers on 0 
#     # - setting divergation point based on Verteilung 
#     temp=dt.from[,list(itemID,returnShipment)]
#     temp=temp[order(itemID,decreasing=TRUE),list(sum(returnShipment == "yes"), length(returnShipment)),by=itemID]
#     #Meine Idee
#     #1
#     temp[V2 >= median(temp$V2), returnItemRateSchatti:=round((V1/V2),digits=2)*100]
#     temp[V2 < median(temp$V2), returnItemRateSchatti:=round((V1 + temp[,round(sum(V1)/sum(V2),digits=2)]*(median(temp$V2)-V1))/median(temp$V2),digits=2)*100]
#     
#     #     #Michaels Entwurf
#     #     #2
#     #     temp[round((V1/V2),digits=2)>=round(sum(temp$V1)/sum(temp$V2),digits=2),
#     #          returnItemRateSimon:=round(V1/(V2+1),digits=2)*100]
#     #     temp[round((V1/V2),digits=2)<round(sum(temp$V1)/sum(temp$V2),digits=2),
#     #          returnItemRateSimon:=round((V1+1)/(V2+1),digits=2)*100]
#     #3,4
#     setnames(temp,c("V1", "V2"),c("itemsReturned","itemsOrdered"))
#     #join der temp an itemID
#     setkey(dt.from,itemID)
#     dt.from<-dt.from[temp]
#     setkey(dt.to,itemID)
#     dt.to<-dt.to[temp]
#     
#     ###manufacturerID##########################################################################
#     temp=dt.from[,list(manufacturerID,returnShipment)]
#     temp=temp[order(manufacturerID,decreasing=TRUE),list(sum(returnShipment == "yes"), 
#                                                          length(returnShipment)),by=manufacturerID]
#     
#     #Meine Idee
#     #1 10 ist beliebig gesetzt
#     temp[V2 >= 10, returnManuRateSchatti:=round((V1/V2),digits=2)*100]
#     temp[V2 < 10, returnManuRateSchatti:=round((V1 + temp[,round(sum(V1)/sum(V2),digits=2)]*(median(temp$V2)-V1))/median(temp$V2),digits=2)*100]
#     
#     #     #Michaels Entwurf
#     #     #2
#     #     temp[round((V1/V2),digits=2)>=round(sum(temp$V1)/sum(temp$V2),digits=2),
#     #          returnManuRateSimon:=round(V1/(V2+1),digits=2)*100]
#     #     temp[round((V1/V2),digits=2)<round(sum(temp$V1)/sum(temp$V2),digits=2),
#     #          returnManuRateSimon:=round((V1+1)/(V2+1),digits=2)*100]
#     #3,4
#     setnames(temp,c("V1", "V2"),c("manuItemsReturned","manuItemsOrdered"))
#     #join temp an manufacturerID
#     setkey(dt.from,manufacturerID)
#     dt.from<-dt.from[temp]
#     setkey(dt.to,manufacturerID)
#     dt.to<-dt.to[temp]
#     
#     #Löschen der temp Tabellen
#     rm(temp)
#     rm(itemPriceRange)
