fs.all <- function(dt) {
    # Remove features not required at all
    dt$customernumber <- NULL
    dt$deliverydatereal <- NULL
    dt$deliverydatepromised <- NULL
    dt$datecreated <- NULL
    dt$points <- NULL
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