#!/usr/bin/env Rscript

source("R/data.R")
source("tools/gritbot/QuinlanAttributes.R")
source("tools/gritbot/formatAttributes.R")
source("tools/gritbot/makeDataFile.R")
source("tools/gritbot/makeNamesFile.R")

cl <- function(x) {
    x$orderDate <- as.numeric(x$orderDate)
    x$deliveryDate <- NULL
    x$dateOfBirth <- NULL
    x$creationDate <- NULL    
    x
}

x <- cl(dt.train.pp)
y <- x$returnShipment
x$returnShipment <- NULL

xt <- cl(dt.test.pp)
yt <- xt$returnShipment
xt$returnShipment <- NULL


write(makeNamesFile(x, y), file="tools/gritbot/dmc.names")
write(makeDataFile(x, y), file="tools/gritbot/dmc.data")
write(makeDataFile(xt, yt), file="tools/gritbot/dmc.test")