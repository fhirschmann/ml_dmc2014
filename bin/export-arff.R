#!/usr/bin/env Rscript
#
# Exports the Data Frame to Weka's ARFF

library(foreign)

source("R/pp.R")
source("R/data.R")
source("R/fs.R")

write.arff(fs.all(dt), "task2010/dmc2010_train.arff")