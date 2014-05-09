#!/usr/bin/Rscript
library(C50)
library(plyr)

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
        #train <- train[1:1000, ]
        if (!customerKnown)
            train <- fs.noCustomer(train)
        train$deliveryDateMissing <- NULL
        
        test <- dt.dmc[[sN]]$test
        orderItemIDmiss <- test[test$deliveryDateMissing == "yes", ]$orderItemID
        test <- test[test$deliveryDateMissing == "no", ]
        orderItemID <- test$orderItemID
        test <- fs.c50(test, customerKnown)
        test$deliveryDateMissing <- NULL
        
        fit <- C5.0(returnShipment ~ ., data=train)
        preds[[N+1]] <- data.frame(orderItemID=orderItemID,
                                   prediction=revalue(predict(fit, test), c("yes"="1", "no"="0")))
        
        scores[[N+1]] <- dmc.score(preds[[N+1]]$prediction, test$returnShipment)
        
        message(paste("Score", sN, scores[[N+1]]))
        message(paste("Accuracy", 1 - scores[[N+1]] / nrow(dt.dmc[[sN]]$test)))
    }
    message(paste("Score", s, scores[[1]] + scores[[2]]))
    message(paste("Accuracy", s, 1 - (scores[[1]] + scores[[2]]) / nrow(dt.dmc[[s]]$test)))
    
    pred <- rbind(rbind(preds[[1]], preds[[2]]),
                  data.frame(orderItemID=orderItemIDmiss,
                             prediction="0"))
    write.table(pred, file=paste(s, "_", "group3_binary.txt", sep=""), sep=";", quote=F, row.names=F)
}