
source("r/data.r")
library(data.table)
library(plyr)
x <- dt.dmc$M30$train[sample(nrow(dt.dmc$M30$train), 2)]



#dt.from <- data.table(x[x$deliveryDateMissing == "no", ])


#orderDates <- unique(dt.from[, c("customerID", "orderDate", "itemID"), with=F])
#setkeyv(orderDates, c("customerID", "orderDate", "itemID"))
#orderDates$C <- paste(orderDates$orderDate, orderDates$customerID, sep="")
##orderDates[, index := 1:.N, by=c("customerID")]
#y <- orderDates[orderDates$customerID == 6]
#mergedItems <- aggregate(itemID ~ C, y, as.vector)
#y$itemID <- NULL
#almostthere <- join(y, mergedItems, by=c("C"))

customerList <- c()
customerItemList <- c()
customerOrderList <- c()
customerSession <- c()

str(customerItemList)

yesorno <- function(customer, item, order) {
  str("incoming")
  str(customer)
 # message("laosdaosdoas")
 ##wenn customer bekannt ..
  if(customer %in% customerList) {
    ##wenn noch nicht bekannte ordersession des customers (festgestellt über orderdate)
    if(!(order %in% customerOrderList[[customer]])) {
      customerOrderList[[customer]] <<- c(customerOrderList[[customer]], order)
      customerSession[[customer]] <<- NULL
    }
    ##wenn item bereits gekauft und das nicht in dieser session, dann TRUE
    if(!(item %in% customerSession[[customer]]) & (item %in% customerItemList[[customer]])) {
      customerSession[[customer]] <<- c(customerSession[[customer]], item)
      TRUE
    } else if (item %in% customerSession[[customer]]){
      str("item in customersession")
      FALSE
    } else {
      str("item not yet in itemlist")
      customerItemList[[customer]] <<- c(customerItemList[[customer]], item)
      FALSE
    }
  } else {
    ##customer ist neu: 
    #eintrag in customerlist anlegen, itemlist anlegen, session anlegen, orderhistory anlegen
    str(item)
    str("and the list")
    str(customerItemList)
    str(customerList)
    customerList <<- c(customerList, customer)
    customerItemList[[customer]] <<- c(customerItemList[[customer]], item)
    customerSession[[customer]] <<- c(item)
    customerOrderList[[customer]] <<- c(list(order), customerOrderList[[customer]] )
    FALSE
  }
}

for(i in 1:nrow(x)) {
  x[i, "test2"] <- yesorno(x[i , ]$customerID, x[i , ]$itemID, x[i , ]$orderDate)
}