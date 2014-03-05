fs.all <- function(dt) {
    # Remove features not required at all
    dt$customernumber <- NULL
    dt$deliverydatereal <- NULL
    dt$deliverydatepromised <- NULL
    dt$datecreated <- NULL
}

fs.c50 <- function(dt) {
    dt2 <- fs.all(dt)

    # C50 specific features
    
    dt2
}

fs.rpart <- function(dt) {
    dt2 <- fs.all(dt)
    
    # rpart specific features
    
    dt2
}