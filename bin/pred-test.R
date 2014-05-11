#!/usr/bin/Rscript
library(C50)
library(plyr)

source("R/data.R")
source("R/fs.R")

train <- dt.dmc$M31$train
test <- dt.dmc$M31$test

train <- train[train$deliveryDateMissing == "no", ]
test <- test[test$deliveryDateMissing == "no", ]

train$customerID <-as.factor(train$customerID)
test$customerID <- as.factor(test$customerID)

train <- fs.c50(train, T)
test <- fs.c50(test, T)

train <- train[1:200, ]
        
fit <- C5.0(returnShipment ~ ., data=train)

preds <- predict(fit, test), c("yes"="1", "no"="0")

score <- dmc.score(preds, test$returnShipment)
message(score)

saveRDS(fit, "c50X.RData")
write.table(preds, "c50X_preds.txt", sep=";", row.names=F, quote=F)
