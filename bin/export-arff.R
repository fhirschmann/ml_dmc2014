#!/usr/bin/env Rscript
library(foreign)
source("R/data.R")

for (n in names(dt.dmc)) {
    train <- dt.dmc[[n]]$train
    test <- dt.dmc[[n]]$test

    write.arff(train, paste("arff/", n, "_train.arff", sep=""))
    write.arff(train[train$deliveryDateMissing == "no", ],
               paste("arff/", n, "_noDDmissing_train.arff", sep=""))

    write.arff(test, paste(n, "_test.arff", sep=""))
    write.arff(test[test$deliveryDateMissing == "no", ],
               paste("arff/", "_noDDmissing_test.arff", sep=""))
}
