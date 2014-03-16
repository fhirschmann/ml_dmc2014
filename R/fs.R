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
    dt2$invoicepostcode <- NULL
    dt2$deliverydatediff <- NULL
    dt2$domain <- NULL
    dt2$title <- NULL
    dt2$gift <- NULL
    dt2$deliverytype <- NULL
    dt2$w8 <- NULL
    dt2$w0all <- NULL
    dt2$week <- NULL
    
    dt2
}
