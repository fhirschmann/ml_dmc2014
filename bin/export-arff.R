#!/usr/bin/env Rscript
library(foreign)
source("R/pp.R")

write.arff(dt2, "task2010/dmc2010_train.arff")