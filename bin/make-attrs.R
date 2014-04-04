#!/usr/bin/env Rscript
# Creates attributes that cannot be created on the fly
# (due to performance reasons or the reliance on both the train and test set).
source("R/data.R")


train.shoeIDs <- unique(dt.train[with(dt.train,
                                       str_sub(size, start=-1, end=-1) == "+"), ]$itemID)
test.shoeIDs <- unique(dt.test[with(dt.test,
                                     str_sub(size, start=-1, end=-1) == "+"), ]$itemID)
shoeIDs <- unique(c(train.shoeIDs, test.shoeIDs))

addattrs <- function(dt, shoeIDs) {
    dtn <- data.frame(row.names=rownames(dt))
    dtn$itemType <- with(dt, cases(
        "pants"=str_length(size) == 4 & size != "XXXL",
        "shoes"=itemID %in% shoeIDs,
        "uni"=size == "UNSIZED",
        "other"=T))
    dtn
}

dt.train2 <- addattrs(dt.train, shoeIDs)
dt.test2 <- addattrs(dt.test, shoeIDs)