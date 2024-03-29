source("./R/colormap.R")

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
    
    ####customer features##########################################################################
    #lifetime returnShipment Rate
    temp<-dt2[,list(itemID,customerID,returnShipment)]
    temp<-temp[order(customerID,decreasing=TRUE),list(sum(returnShipment == "yes"),
                                                           length(itemID)),by=list(customerID)]
    #1 custReturnRateSchatti - Meine Idee
    temp[V2 >= median(temp$V2), custReturnRateSchatti:=round((V1/V2),digits=2)*100]
    temp[V2 < median(temp$V2), custReturnRateSchatti:=round((V1 + temp[,round(sum(V1)/sum(V2),digits=2)]*(median(temp$V2)-V1))/median(temp$V2),digits=2)*100]
    
    #2 custReturnRateSimon - Michaels Entwurf
    temp[round((V1/V2),digits=2)>=round(sum(temp$V1)/sum(temp$V2),digits=2),
         custReturnRateSimon:=round(V1/(V2+1),digits=2)*100]
    temp[round((V1/V2),digits=2)<round(sum(temp$V1)/sum(temp$V2),digits=2),
         custReturnRateSimon:=round((V1+1)/(V2+1),digits=2)*100]
    #3,4
    setnames(temp,c("V1", "V2"),c("custItemsReturned","custItemsOrdered"))
    #join die temp an customerID
    setkey(dt2,customerID)
    dt2<-dt2[temp]
    
    ###price features##########################################################################
    temp<-dt2[,list(itemID,price,orderDate,size,color)]
    #1,2 ->min and max price
    itemPriceRange<- temp[,list(min(price),max(price)),by=list(itemID)]
    setnames(itemPriceRange,c("V1", "V2"),c("min","max"))
    #3 -> quantity of price levels
    setkey(itemPriceRange,itemID)
    itemPriceRange=itemPriceRange[temp[order(itemID,-price,decreasing=FALSE),length(itemID),by=list(itemID,price)][,length(price),by=itemID]]
    setnames(itemPriceRange,"V1","levels")
    #4 -> boolean price range TRUE FALSE
    itemPriceRange[,priceRange:=0] #hinzufügen Feature priceRange
    itemPriceRange[max!=min,priceRange:=1] #0:keine Range       1:Range
    #5 -> absolute Price Range
    itemPriceRange[,AbsPriceRange:=max-min] #hinzufügen Feature abs priceDiff
    
    #join itemPriceRange an itemID
    setkey(dt2,itemID)
    dt2<-dt2[itemPriceRange]
    #6 -> actual discount on piece
    dt2[, actDiscount := round(1-(price/max),digits=2)*100]
    #7 -> boolean discounted TRUE False
    dt2$discount <- as.factor(ifelse(dt2$actDiscount==0, "no", "yes"))
    
    ###itemID features##########################################################################
    #return rate of items - hight rollers setting a fetaure on 1 low rollers on 0 
    # - setting divergation point based on Verteilung 
    temp=dt2[,list(itemID,returnShipment)]
    temp=temp[order(itemID,decreasing=TRUE),list(sum(returnShipment == "yes"), length(returnShipment)),by=itemID]
    #Meine Idee
    #1
    temp[V2 >= median(temp$V2), returnItemRateSchatti:=round((V1/V2),digits=2)*100]
    temp[V2 < median(temp$V2), returnItemRateSchatti:=round((V1 + temp[,round(sum(V1)/sum(V2),digits=2)]*(median(temp$V2)-V1))/median(temp$V2),digits=2)*100]
    
    #Michaels Entwurf
    #2
    temp[round((V1/V2),digits=2)>=round(sum(temp$V1)/sum(temp$V2),digits=2),
         returnItemRateSimon:=round(V1/(V2+1),digits=2)*100]
    temp[round((V1/V2),digits=2)<round(sum(temp$V1)/sum(temp$V2),digits=2),
         returnItemRateSimon:=round((V1+1)/(V2+1),digits=2)*100]
    #3,4
    setnames(temp,c("V1", "V2"),c("itemsReturned","itemsOrdered"))
    #join der temp an itemID
    setkey(dt2,itemID)
    dt2<-dt2[temp]
    
    ###manufacturerID##########################################################################
    temp=dt2[,list(manufacturerID,returnShipment)]
    temp=temp[order(manufacturerID,decreasing=TRUE),list(sum(returnShipment == "yes"), 
                                            length(returnShipment)),by=manufacturerID]
    
    #Meine Idee
    #1 10 ist beliebig gesetzt
    temp[V2 >= 10, returnManuRateSchatti:=round((V1/V2),digits=2)*100]
    temp[V2 < 10, returnManuRateSchatti:=round((V1 + temp[,round(sum(V1)/sum(V2),digits=2)]*(median(temp$V2)-V1))/median(temp$V2),digits=2)*100]
    
    #Michaels Entwurf
    #2
    temp[round((V1/V2),digits=2)>=round(sum(temp$V1)/sum(temp$V2),digits=2),
         returnManuRateSimon:=round(V1/(V2+1),digits=2)*100]
    temp[round((V1/V2),digits=2)<round(sum(temp$V1)/sum(temp$V2),digits=2),
         returnManuRateSimon:=round((V1+1)/(V2+1),digits=2)*100]
    #3,4
    setnames(temp,c("V1", "V2"),c("manuItemsReturned","manuItemsOrdered"))
    #join temp an manufacturerID
    setkey(dt2,manufacturerID)
    dt2<-dt2[temp]
    
    #Löschen der temp Tabellen
    rm(temp)
    rm(itemPriceRange)
    
    dt2
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
    dt.from <- data.table(from)
    
    dt.from[, customerReturnRate := sum(returnShipment == "yes") / .N, by=c("customerID")]
    customerRetRate <- unique(dt.from[, c("customerID", "customerReturnRate"), with=F])
    dt.to <- join(dt.to, customerRetRate, by="customerID")
    
    dt.from[, itemReturnRate := sum(returnShipment == "yes") / .N, by=c("itemID")]
    itemRetRate <- unique(dt.from[, c("itemID", "itemReturnRate"), with=F])
    dt.to <- join(dt.to, itemRetRate, by="itemID")
    
    dt.to
}