fs.all <- function(dt) {
    dt2 <- dt
    
    dt2$orderItemID <- NULL
    
    dt2$orderDate <- NULL
    
    # Use deliveryTime
    dt2$deliveryDate <- NULL
    
    # Use customerAge
    dt2$dateOfBirth <- NULL
    
    # Use accountAge
    dt2$creationDate <- NULL
    dt2$firstOrderDate <- NULL
    
    # We remove these instances beforehand
    dt2$deliveryDateMissing <- NULL
    
    dt2
}

fs.svm <- function(dt) {
    dt2 <- fs.all(dt)
    
    dt2$discretizedPrice <- NULL
    dt2$discretizedCustomerAge <- NULL
    dt2$discretizedAccountAge <- NULL
    dt2$size <- NULL
    
    dt2
}

fs.nb <- function(dt) {
    dt2 <- fs.all(dt)
    
    dt2$discretizedPrice <- NULL
    dt2$discretizedCustomerAge <- NULL
    dt2$discretizedAccountAge <- NULL
    dt2$size <- NULL

    dt2[,eval(c("returnShipment", "customerReturnRate", "itemReturnRate", "price"))]
}

fs.c50 <- function(dt) {
    dt2 <- fs.all(dt)
    
    #dt2$discretizedPrice <- NULL
    dt2$discretizedCustomerAge <- NULL
    dt2$discretizedAccountAge <- NULL
    
    dt2
}

fs.gbm <- function(dt) {
    dt2 <- fs.tree(dt)
    
    dt2$color <- NULL
    dt2$instantOrder <- NULL
    dt2$salutation <- NULL
    dt2$holiday <- NULL

    
    dt2
}


fs.tree <- function(dt) {
    dt2 <- fs.all(dt)
    
    dt2$price <- NULL  # Use discretized price
    dt2$customerAge <- NULL  # Same here
    dt2$accountAge <- NULL
    
    dt2
}

fs.nn <- function(dt) {
    dt2 <- fs.all(dt)
    
    dt2$discretizedPrice <- NULL
    dt2$discretizedCustomerAge <- NULL
    dt2$discretizedAccountAge <- NULL
    
    dt2
}

fs.rf <- function(dt) {
    require(randomForest)
    dt2 <- fs.tree(dt)
    
    dt2 <- na.roughfix(dt2)
    
    dt2
}