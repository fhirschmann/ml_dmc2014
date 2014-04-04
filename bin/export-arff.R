#!/usr/bin/env Rscript
#
# Exports the Data Frame to Weka's ARFF

library(foreign)

source("R/data.R")
source("R/fs.R")

write.arff(fs.all(dt.train), "task/orders_train.arff")