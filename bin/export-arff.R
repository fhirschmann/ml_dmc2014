#!/usr/bin/env Rscript
#
# Exports the Data Frame to Weka's ARFF

library(foreign)

source("R/pp.R")

write.arff(dt2, "task2010/dmc2010_train.arff")