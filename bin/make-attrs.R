#!/usr/bin/env Rscript
# Creates attributes that cannot be created on the fly
# (due to performance reasons or the reliance on both the train and test set).
source("R/data.R")

# Frequency of the customer ID (corresponds to the number of items ordered per customer)
customerID.freq <- data.frame(table(factor(c(as.character(dt.train$customerID),
                                             as.character(dt.test$customerID)))))
colnames(customerID.freq) <- c("customerID", "Freq")
customerID.freq <- customerID.freq[order(customerID.freq$Freq, decreasing=T), ]
rownames(customerID.freq) <- 1:nrow(customerID.freq)

write.csv(customerID.freq, file="task/customerid_freq.csv", quote=F)

# Attribute itemType
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