#!/usr/bin/Rscript
library(C50)

source("R/data.R")
source("R/fs.R")

for (s in c("M1", "M2", "M3")) {
    preds <- list()
    scores <- list()
    
    for (N in 0:1) {
        sN <- paste(s, N, sep="")
        
        message(paste("Training", sN))
        
        customerKnown <- sN %in% c("M10", "M20", "M30")
        
        train <- dt.dmc[[sN]]$train
        train <- train[train$deliveryDateMissing == "no", ]
        train <- fs.c50(train, customerKnown)
        train$deliveryDateMissing <- NULL
        
        test <- dt.dmc[[sN]]$test
        test <- test[test$deliveryDateMissing == "no", ]
        test <- fs.c50(test, customerKnown)
        test$deliveryDateMissing <- NULL
        
        fit <- C5.0(returnShipment ~ ., data=train)
        preds[[N]] <- predict(fit, test)
        
        scores[[N]] <- dmc.score(preds, t$returnShipment)
        
        message(paste("Score", sN, scores[[N]]))
    }
    message(paste("Score", s, scores[[1]] + scores[[2]]))
}