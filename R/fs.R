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
    
    dt2
}

fs.tree <- function(dt) {
    dt2 <- fs.all(dt)
    dt2$price <- NULL
    
    dt2
}