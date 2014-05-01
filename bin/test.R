#!/usr/bin/env Rscript

source("R/data.R")
source("R/fs.R")
source("R/dmc.R")
library(caret)

dt.train <- dt.dmc$T1$train
dt.test <- dt.dmc$T1$test

use <- dt.train$deliveryDateMissing == "no"

fit <- train(returnShipment ~ ., fs.tree(dt.train[use, ]),
             method="C5.0", tuneGrid=expand.grid(trials=1, model="rules", winnow=F),
             na.action=na.pass, trControl=trainControl(method="none"))


pred <- dt.test
pred$pred <- predict(fit, dt.test, na.action=na.pass)
pred[pred$deliveryDateMissing == "yes", ]$pred <- "no"

score <- dmc.score(pred$pred, pred$returnShipment)
score

acc <- 1 - score / nrow(dt.test)
acc
