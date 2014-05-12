#!/usr/bin/Rscript
library(C50)
library(plyr)

source("R/data.R")
source("R/fs.R")
source("R/utils.R")

train <- dt.dmc$M31$train
test <- dt.dmc$M31$test

train <- train[train$deliveryDateMissing == "no", ]
test <- test[test$deliveryDateMissing == "no", ]

#train$customerID <-as.factor(train$customerID)
#test$customerID <- as.factor(test$customerID)

levels(train$customerID) <- c(levels(train$customerID), levels(test$customerID))

train <- fs.c50(train, T)
test <- fs.c50(test, T)

#train <- train[1:200, ]

train <- addlevels(train, test)
test <- addlevels(test, train)
        
fit <- C5.0(returnShipment ~ ., data=train)
saveRDS(fit, "fit.RData")

preds <- predict(fit, test)

score <- dmc.score(preds, test$returnShipment)
message(score)

saveRDS(fit, "c50X.RData")
write.table(preds, "c50X_preds.txt", sep=";", row.names=F, quote=F)
