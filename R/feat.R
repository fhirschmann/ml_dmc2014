source("R/colormap.R")

add.features <- function(dt) {
    #
    # Place features that can be computed over the entire data set here.
    #
    require(plyr)
    
    dt2 <- data.table(dt)
    
    dt2$creationDateMissing <- as.factor(ifelse(is.na(dt2$creationDate), "yes", "no"))
    
    dt2$deliveryTime <- as.integer(dt2$deliveryDate - dt2$orderDate)
    dt2$deliveryDateMissing <- as.factor(ifelse(is.na(dt2$deliveryDate), "yes", "no"))
    dt2$orderWeekday <- as.ordered(as.factor(lubridate::wday(dt2$orderDate, label=T, abbr=F)))
    
    # Instant Order
    dt2$instantOrder <- as.factor(ifelse(dt2$creationDate == dt2$orderDate, "yes", "no"))
    
    # Customer age in Years
    dt2$customerAge <- as.integer(year(dt2$orderDate) - year(dt2$dateOfBirth))
    dt2$discretizedCustomerAge <- cut(dt2$customerAge, 10 + 1:15 * 5)
    dt2$dateOfBirthMissing <- as.factor(ifelse(is.na(dt2$dateOfBirth), "yes", "no"))
    
    # Account age in Days
    dt2$accountAge <- as.numeric(dt2$orderDate - dt2$creationDate)
    dt2$discretizedAccountAge <- cut(dt2$accountAge, c(-1, 1:16 * 50))
    
    # Number of items ordered with the same ID
    dt2[, sameItemsOrdered := .N, by=c("itemID", "customerID", "orderDate")]
    dt2$sameItemsOrderedBool <- as.factor(ifelse(dt2$sameItemsOrdered > 2, "yes", "no"))
    
    # Total number of items ordered
    dt2[, customerNumItemsOrdered := .N, by=c("itemID", "customerID", "orderDate")]
    
    # Total number of distinct items ordered
    dt2[, customerDistinctItemsOrdered := length(unique(itemID)), by=c("customerID", "orderDate")]
    dt2$customerDistinctItemsOrderedBool <- as.factor(ifelse(dt2$customerDistinctItemsOrdered > 1, "yes", "no"))
    
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
    
    dt2[is.na(dt2$itemDiscount), c("itemDiscount")] <- 0
    
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
    
    message(paste("Calculating features for", nrow(to), "instances based on", nrow(from), "instances"))
    
    dt.to <- data.table(to)
    dt.from <- data.table(from[from$deliveryDateMissing == "no", ])
    
    dt.from[, customerReturnRate := lsmooth(sum(returnShipment == "yes"), .N), by=c("customerID")]
    customerRetRate <- unique(dt.from[, c("customerID", "customerReturnRate"), with=F])
    dt.to <- join(dt.to, customerRetRate, by="customerID")
    
    # TODO: Maybe we should group this by c("itemID", "color", "size")
    dt.from[, itemReturnRate := lsmooth(sum(returnShipment == "yes"), .N), by=c("itemID", "size")]
    itemRetRate <- unique(dt.from[, c("itemID", "itemReturnRate", "size"), with=F])
    dt.to <- join(dt.to, itemRetRate, by=c("itemID", "size"))
    
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
    
    dt.to
}