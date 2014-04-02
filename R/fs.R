fs.all <- function(dt) {
    dt2 <- dt

    dt2$customerID <- NULL
    dt2$orderDate <- NULL
    dt2$deliveryDate <- NULL
    dt2$dateOfBirth <- NULL
    dt2$creationDate <- NULL
    
    dt2
}