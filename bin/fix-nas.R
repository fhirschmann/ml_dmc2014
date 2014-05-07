#!/usr/bin/env Rscript
#
# Missing Value Imputation

#library(devtools)
#install_github("imputation", "jeffwong")
library(imputation)
library(zoo)
library(caret)
library(lubridate)
library(gbm)
library(data.table)

source("R/data.R")
source("R/fs.R")

nas <- function(x) {
    y <- sapply(x, function(z) sum(is.na(z)))
    y[y > 0]
}

dt.train$returnShipment <- NULL

# Comment the next two lines out!
dt.train <- dt.train[1:100,]
dt.test <- dt.test[1:100,]

data <- rbind(dt.train, dt.test)

data$color <- as.character(data$color)
data$fewcolors <- as.character(data$fewcolors)

data[is.na(data$color), c("color", "fewcolors")] <- "MISSING"
data$color <- as.factor(data$color)
data$fewcolors <- as.factor(data$fewcolors)

orderDate <- data$orderDate
creationDate <- data$creationDate
train.orderItemID <- dt.train$orderItemID
test.orderItemID <- dt.test$orderItemID

data <- data.frame(fs.all(data))

zeroVar <- names(which(sapply(data, function(x) length(unique(x)) == 1)))
data <- data[!names(data) %in% zeroVar]


nas(data)
nas.deliveryTime <- is.na(data$deliveryTime)
nas.train.deliveryTime <- is.na(dt.train$deliveryTime)
nas.test.deliveryTime <- is.na(dt.test$deliveryTime)
nas.accountAge <- is.na(data$accountAge)
nas.customerAge <- is.na(data$customerAge)

ctrl <- trainControl(method="cv", number=10, savePredictions=T)
grid <- expand.grid(
    shrinkage=0.1,
    interaction.depth=1:3,
    n.trees=c(1:20 * 20)
)

grid2 <- expand.grid(shrinkage=0.1,
                     interaction.depth=1:10,
                     n.trees=1:10 * 40)

grid3 <- expand.grid(shrinkage=0.8, interaction.depth=1, n.trees=1)

# deliveryTime
train.deliveryTime <- train(deliveryTime ~ ., data=data[!nas.deliveryTime, ],
                            method="gbm", tuneGrid=grid3, na.action=na.omit)
train.deliveryTime

preds.train.deliveryTime <- as.integer(predict(train.deliveryTime, dt.train[nas.train.deliveryTime, ],
                                               na.action=na.pass))
preds.train.deliveryDate <- orderDate[nas.train.deliveryTime] + days(preds.train.deliveryTime)
df.train.deliveryDate <- data.table(orderItemID=train.orderItemID[nas.train.deliveryTime],
                                    deliveryDate=preds.train.deliveryDate)

preds.test.deliveryTime <- as.integer(predict(train.deliveryTime, dt.test[nas.test.deliveryTime, ],
                                               na.action=na.pass))
preds.test.deliveryDate <- orderDate[nas.test.deliveryTime] + days(preds.test.deliveryTime)
df.test.deliveryDate <- data.table(orderItemID=test.orderItemID[nas.test.deliveryTime],
                                    deliveryDate=preds.test.deliveryDate)

# accountAge
train.accountAge <- train(accountAge ~ ., data=data[!nas.accountAge, ],
                          method="gbm", tuneGrid=grid, na.action=na.omit)
train.accountAge

preds.accountAge <- as.integer(predict(train.accountAge, data[nas.accountAge, ],
                                       na.action=na.pass))
preds.creationDate <- orderDate[nas.accountAge] - days(preds.accountAge)

## Take the median of the creationDate per customer
df.creationDate <- data.table(customerID=data[nas.accountAge, ]$customerID,
                              creationDate2=preds.creationDate)
df.creationDate[, creationDate := median(creationDate2), by=c("customerID")]
df.creationDate$creationDate2 <- NULL
df.creationDate <- unique(df.creationDate)

# customerAge
train.customerAge <- train(customerAge ~ ., data=data[!nas.customerAge, ],
                           method="gbm", na.action=na.pass)
train.customerAge

preds.customerAge <- as.integer(predict(train.customerAge, data[nas.customerAge, ],
                                        na.action=na.pass))
preds.dateOfBirth <- orderDate[nas.customerAge] - years(preds.customerAge)
df.dateOfBirth <- data.table(customerID=data[nas.customerAge, ]$customerID,
                             dateOfBirth2=preds.dateOfBirth)
df.dateOfBirth[, dateOfBirth := median(dateOfBirth2), by=c("customerID")]
df.dateOfBirth$dateOfBirth2 <- NULL
df.dateOfBirth <- unique(df.dateOfBirth)

saveRDS(list(accountAge=train.accountAge,
             customerAge=train.customerAge,
             deliverTime=train.deliveryTime),
        file="data/imputation.RData")

write.table(df.dateOfBirth, file="data/na.dateOfBirth.txt", sep=";", quote=F, row.names=F)
write.table(df.creationDate, file="data/na.creationDate.txt", sep=";", quote=F, row.names=F)
write.table(df.train.deliveryDate, file="data/na.deliveryDate_train.txt", sep=";", quote=F, row.names=F)
write.table(df.test.deliveryDate, file="data/na.deliveryDate_class.txt", sep=";", quote=F, row.names=F)