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
    dt2$colors <- NULL
    dt2$instantorder <- NULL
    
    dt2$orderItemID <- NULL  # This attribute is useless
    
    dt2
}