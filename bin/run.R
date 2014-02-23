#!/usr/bin/env Rscript
### This runs the pipeline ###
library(caret)
library(ROCR)

source("R/common.R")
source("R/pipeline.R")

for (name in names(trainers)) {
    fit <- trainers[[name]](voucher ~ ., data=dt2)
    
    # Save model to a file.
    fname <- file.path("models", paste(name, ".RData", sep=""))
    save(fit, file=fname)
    
    print(fit)
}