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
    dt2$customerFirstOrderDate <- NULL
    
    # We remove these instances beforehand
    dt2$deliveryDateMissing <- NULL
    
    dt2
}

fs.noCustomer <- function(dt) {
    dt2 <- dt
    
    dt2$customerID <- NULL
    dt2$customerMoneySpent <- NULL
    dt2$customerItemIsFavoriteBaseColor <- NULL
    dt2$customerFavoriteBaseColor <- NULL
    dt2$customerItemIsFavoriteColor <- NULL
    dt2$customerFavoriteColor <- NULL
    dt2$customerNumOrders <- NULL
    dt2$customerAccountAge <- NULL
    dt2$customerAccountAgeAtOrderTime <- NULL
    dt2$customerAgeAtOrderTimeDiscrete <- NULL
    dt2$customerAccountAgeAtOrderTimeDiscrete <- NULL
    dt2$customerAccountAgeDiscrete <- NULL
    
    # THIS SHOULD NOT BE HERE (rename this)
    dt2$customerNumItemsOrdered <- NULL
    dt2$customerReturnRate <- NULL
    dt2$customerTotalOrderVolume <- NULL
    
    dt2
}

fs.noDiscrete <- function(dt) {
    rm <- names(unlist(sapply(colnames(dt2),
                              function(x) as.logical(grep("Discrete", x)), simplify=T)))
    dt[setdiff(colnames(dt), rm)]
}

fs.onlyDiscrete <- function(dt) {
    require(stringr)
    
    keep <- names(unlist(sapply(colnames(dt),
                                function(x) as.logical(grep("Discrete", x)), simplify=T)))
    rm <- str_sub(keep, 1, -9)
    dt[setdiff(colnames(dt), rm)]
}

fs.svm <- function(dt) {
    dt2 <- fs.noDiscrete(fs.all(dt))
    
    dt2$size <- NULL
    
    dt2
}

fs.gbm <- function(dt) {
    dt2 <- fs.tree(dt)
    
    dt2
}

fs.tree <- function(dt) {
    dt2 <- fs.onlyDiscrete(fs.all(dt))
    
    dt2
}

fs.c50 <- function(dt, customerKnown) {
    dt2 <- fs.tree(dt)
    
    if (customerKnown) {
        dt2$itemID <- NULL
        dt2$color <- NULL
        dt2$sizeReturnRate <- NULL
        dt2$baseColorReturnRate <- NULL
        dt2$customerNumItemsOrdered <- NULL
        dt2$westGermany <- NULL
        dt2$itemPriceLevelsGreater1 <- NULL
        dt2$orderWeekday <- NULL
        dt2$baseColor <- NULL
        #dt2$customerID <- NULL
    } else {
        dt2$itemID <- NULL
        dt2$sizeReturnRate <- NULL
        dt2$baseColor <- NULL
        dt2$colorReturnRate <- NULL
        dt2$dateOfBirthMissing <- NULL
        dt2$westGermany <- NULL
        dt2$unknownManufacturer <- NULL
        dt2$manufacturerReturnRate <- NULL
        dt2$baseColorReturnRate <- NULL
    }
        
    dt2
}

fs.nn <- function(dt) {
    dt2 <- fs.onlyDiscrete(fs.all(dt))
    
    dt2
}

fs.rf <- function(dt) {
    require(randomForest)
    dt2 <- fs.tree(dt)
    
    dt2 <- na.roughfix(dt2)
    
    dt2
}

fs.simon <- function(dt) {
  dt2 <- fs.gbm(dt)
  
  ##wir lassen itemId nicht drin!
  dt2$itemID <- NULL
  
  
  dt2
  
}

fs.nb <- function(dt) {
  require(randomForest)
  dt2 <- fs.all(dt)
  
  dt2 <- na.roughfix(dt2)
  
  
  dt2
}