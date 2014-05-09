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
        
        customerKnown <- sN %in% c("M11", "M21", "M31")
        
        train <- dt.dmc[[sN]]$train
        train <- train[train$deliveryDateMissing == "no", ]
        train <- fs.c50(train, customerKnown)
        if (!customerKnown)
            train <- fs.noCustomer(train)
        train$deliveryDateMissing <- NULL
        
        test <- dt.dmc[[sN]]$test
        test <- test[test$deliveryDateMissing == "no", ]
        test <- fs.c50(test, customerKnown)
        test$deliveryDateMissing <- NULL
        
        fit <- C5.0(returnShipment ~ ., data=train)
        preds[[N+1]] <- predict(fit, test)
        
        scores[[N+1]] <- dmc.score(preds[[N+1]], test$returnShipment)
        
        message(paste("Score", sN, scores[[N+1]]))
        message(paste("Accuracy", 1 - scores[[N+1]] / nrow(dt.dmc[[sN]]$test)))
    }
    message(paste("Score", s, scores[[1]] + scores[[2]]))
    message(paste("Accuracy", s, 1 - (scores[[1]] + scores[[2]]) / nrow(dt.dmc[[s]]$test)))
}