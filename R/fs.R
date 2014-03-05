fs.all <- function(dt) {
    dt2 <- dt
    # Remove features not required at all
    dt2$customernumber <- NULL
    dt2$date <- NULL
    dt2$deliverydatereal <- NULL
    dt2$deliverydatepromised <- NULL
    dt2$datecreated <- NULL
    dt2$points <- NULL
    dt2$advertisingdatacode <- NULL
    dt2$delivpostcode <- NULL
    dt2$deliverydatediff <- NULL
    dt2
}

fs.c50 <- function(dt) {
    dt2 <- fs.all(dt)

    # C50 specific features
    
    dt2
}

fs.cart <- function(dt) {
    dt2 <- fs.all(dt)
    
    # cart specific features
    
    dt2
}