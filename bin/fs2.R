#!/usr/bin/env Rscript
source("R/dmc.R")
source("R/utils.R")
source("R/data.R")
source("R/fs.R")

data.name <- "T1"

data <- readRDS("data.dmc.RData")[[data.name]]

# Take a sample
data.samp <- list(train=data$train[sample(nrow(data$train), 2000), ],
                  test=data$test[sample(nrow(data$test), 100), ])

message("erster train")
exclude <- c() # greedy
train <- dmctrain(data.samp, data.name, fs.tree, method="C5.0",
                  save.path=NULL, verbose=F,
                  tuneGrid=expand.grid(trials=1, winnow=F, model="tree"))
acc.max <- train$bestResults$accuracy
rm(train)

str(train)

message("und looos")

for (p in colnames(data$train)) {    
    train <- dmctrain(data.samp, data.name, function(x) x[!names(x) %in% c(exclude, p)], method="C5.0",
                      save.path=NULL, verbose=F,
                      tuneGrid=expand.grid(trials=1, winnow=F, model="tree"))$bestResults$accuracy
    acc.now <- train$bestResults$accuracy
    rm(train)
    
    change <- acc.now - acc.max
    message(paste("Removing predictor ", p, ". Change: ", change,
        (if (change >= 0) " (removed)" else " (not removed)"), sep=""))
    if (change >=0) {
        exclude <- c(exclude, p)
    }
}