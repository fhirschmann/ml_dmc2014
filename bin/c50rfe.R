#!/usr/bin/env Rscript
library(C50)
source("R/data.R")

m <- fs.noCustomer(fs.tree(dt.dmc$M10$train))
t <- fs.noCustomer(fs.tree(dt.dmc$M10$test))

exclude <- c()

fuck <- function(dt) {
    fit <- C5.0(returnShipment ~ ., data=dt)
    preds <- predict(fit, t)
    dmc.score(preds, t$returnShipment)
}

score.min <- fuck(m)
message("All Features:")
message(paste("\t Score", score.min))

for (f in colnames(m)) {
    if (f != "returnShipment") {
        exclude2 <- c(exclude, f)
        message(paste("Excluding:", paste(exclude2, collapse=",")))
        
        score <- fuck(m[!colnames(m) %in% exclude2])
        message(paste("\tScore: ", score))
        if (score <= score.min) {
            score <- score.min
            exclude <- c(exclude, f)
            message(paste("Remove:", f))
        } else {
            message(paste("Don't Remove:", f))
        }
    }
}
