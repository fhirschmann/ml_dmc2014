fs.all <- function(dt) {
    dt2 <- dt
    
    dt2$orderItemID <- NULL
    
    dt2$orderDate <- NULL
    
    # Use deliveryTime
    dt2$orderDeliveryDate <- NULL
    
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
    message("")
    message("EXCLUDING CUSTOMER FEATURES (UNKNOWN)")
    message("")
    dt2 <- dt
    
    dt2$customerID <- NULL
    dt2$customerMoneySpent <- NULL
    
    dt2$customerReturnRate <- NULL
    dt2$customerTotalOrderVolume <- NULL
    
    dt2$customerFavoriteColor <- NULL
    dt2$customerFavoriteBaseColor <- NULL
    
    dt2$customerItemIsFavoriteBaseColor <- NULL
    dt2$customerItemIsFavoriteColor <- NULL
    
    dt2$customerNumOrders <- NULL
    
    dt2$customerFavoriteSize <- NULL
    dt2$customerItemIsFavoriteSize <- NULL
    dt$customerDiscountTendency <- NULL
    
    dt2
}

fs.noDiscrete <- function(dt) {
    rm <- names(unlist(sapply(colnames(dt),
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

    dt2
}

fs.stat <- function(dt) {
    require(randomForest)
    
    dt2 <- fs.noDiscrete(fs.all(dt))
    dt2 <- na.roughfix(dt2)
    
    dt2
}

fs.gbm <- function(dt, customerKnown) {
    dt2 <- fs.tree(dt)

    if (customerKnown) {
        dt2$customerItemIsFavoriteColor <- NULL
        dt2$customerID <- NULL
        dt2$itemColorReturnRate <- NULL
    } else {
        dt2$salutation <- NULL
        dt2$itemSizeReturnRate <- NULL
        dt2$customerState <- NULL
        dt2$orderNumDistinctItems <- NULL
    }
    
    dt2
}

fs.tree <- function(dt) {
    dt2 <- fs.onlyDiscrete(fs.all(dt))
    
    dt2
}

fs.c50 <- function(dt, customerKnown) {
    dt2 <- fs.tree(dt)
    
    dt2$itemColor <- NULL
    dt2$itemSizeReturnRate <- NULL
    dt2$dateOfBirthMissing <- NULL
    
    if (customerKnown) {
        
    } else {
        # Run again on M3
        dt2$customerState <- NULL
        dt2$itemSize <- NULL
        dt2$customerWestGerman <- NULL
        dt2$holiday <- NULL
        dt2$orderItemNumSameItemsOrderedColor <- NULL
        dt2$itemPriceRange
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

fs.simon <- function(dt) {
  dt2 <- dt
  dt2$itemID <- NULL
  dt2$manufacturerID <- NULL
  dt2$customerID <- NULL
  dt2$salutation <- NULL
  dt2$holiday <- NULL
  dt2$dateOfBirthMissing <- NULL
  dt2$dateOfBirthIsOutlier <- NULL
  dt2$deliveryDateIsOutlier <- NULL
  dt2$creationDateIsOutlier <- NULL
  dt2$customerState <- NULL
  dt2$orderDeliveryTime <- NULL
  dt2$orderWeekday <- NULL
  dt2$orderInstant <- NULL
  dt2$customerAgeAtOrderTime <- NULL
  dt2$customerAgeAtOrderTimeDiscrete <- NULL
  dt2$customerAccountAgeAtOrderTime <- NULL
  dt2$customerAccountAgeAtOrderTimeDiscrete <- NULL
  dt2$customerAccountAge <- NULL
  dt2$customerAccountAgeDiscrete <- NULL
  dt2$customerWestGerman <- NULL
  dt2$customerFavoriteColor <- NULL
  dt2$customerItemIsFavoriteColor <- NULL
  dt2$itemItemColorReturnRate <- NULL
  dt2$itemColorReturnRate <- NULL
  
  dt2 <- fs.stat(dt2)
  
  dt2
  
  

  
}

fs.ensemble <- function(dt) {
  
  library(plyr)
  
  ###put all prediction txt reads here:
  ##all __0 (customerUnknown) here:
  #M__:
  delayedAssign("c50M30tr", read.table("ens/c50C_M30_pred.txt", sep=";", header=T, col.names=c("orderItemID", "c50")))
  delayedAssign("pdaM30tr", read.table("ens/pdaC_M30_prob.txt", sep=";", header=T, col.names=c("orderItemID", "pda")))
  delayedAssign("gbmM30tr", read.table("ens/gbmCs5_M30_prob.txt", sep=";", header=T, col.names=c("orderItemID", "gbm")))
  
  #F__:
  
  #B__:
  
  ##all __1 (customerKnown) here:
  #M__:
  
  #F__:
  
  #B__:
  
  
  ###end prediction txt reads
  
  ##insert delayedAssign objects here accordingly:
  ens <- list(
    M0=list(
      c50M30tr,
      pdaM30tr,
      gbmM30tr
    ),
    M1=list(
      
    ),
    F0=list(
    ),
    F1=list(
    ),
    B0=list(
    ),
    B1=list(
    )
  )
  
  library(stringr)
  objName <- args[[2]]
  objName <- tolower(objName)
  lastChar <- str_sub(objName, start=-1)
  firstChar <- str_sub(objName, end=1)

  toJoin <- dt
  
  if(firstChar == "m") {
    if(lastChar == "1"){
      for(i in ens$M1) {
        toJoin <- join(toJoin, i)
      }
      
    } else {
      for(i in ens$M0) {
        toJoin <- join(toJoin, i)
      }
    }    
  } else if(firstChar == "f"){
    if(lastChar == "1"){
      for(i in ens$F1) {
        toJoin <- join(toJoin, i)
      }
    } else {
      for(i in ens$F0) {
        toJoin <- join(toJoin, i)
      }
    }
  } else { #is b
    if(lastChar == "1"){
      for(i in ens$B1) {
        toJoin <- join(toJoin, i)
      }
    } else {
      for(i in ens$B0) {
        toJoin <- join(toJoin, i)
      }
    }
  }
  
  toJoin <- fs.c50(toJoin,FALSE)
  

  toJoin

}
