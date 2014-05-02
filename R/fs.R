fs.all <- function(dt) {
    dt2 <- dt

    #dt2$customerID <- NULL
    dt2$orderDate <- NULL
    dt2$deliveryDate <- NULL
    dt2$dateOfBirth <- NULL
    dt2$creationDate <- NULL
    dt2$itemID <- NULL
    dt2$manufacturerID <- NULL
    dt2$customerID <- NULL
    dt2$color <- NULL
    
    dt2$firstOrderDate <- NULL
    
    dt2$orderItemID <- NULL  # This attribute is useless
    
    dt2$deliveryDateMissing <- NULL
    
    dt2
}

fs.svm <- function(dt) {
    dt2 <- fs.all(dt)
    
    dt2$discretizedPrice <- NULL
    dt2$size <- NULL
    
    dt2
}

fs.nb <- function(dt) {
    dt2 <- fs.all(dt)
    
    dt2$discretizedPrice <- NULL
    dt2$size <- NULL
    
    droplevels(dt2)
}


fs.tree <- function(dt) {
    dt2 <- fs.all(dt)
    
    dt2$price <- NULL
    
    dt2
}

fs.nn <- function(dt) {
    dt2 <- fs.all(dt)
    
    dt2$discretizedPrice <- NULL
    
    dt2
}

fs.rf <- function(dt) {
    require(randomForest)
    dt2 <- fs.tree(dt)
    
    dt2 <- na.roughfix(dt2)
    
    dt2
}