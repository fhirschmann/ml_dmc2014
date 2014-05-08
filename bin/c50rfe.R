#!/usr/bin/env Rscript
library(C50)
source("R/data.R")

m <- fs.noCustomer(fs.tree(dt.dmc$M10$train))
t <- fs.noCustomer(fs.tree(dt.dmc$M10$test))

exclude <- c()
keep <- c("returnShipment", "size")

fuck <- function(dt) {
    fit <- C5.0(returnShipment ~ ., data=dt)
    preds <- predict(fit, t)
    dmc.score(preds, t$returnShipment)
}

score.min <- fuck(m)
message(paste("Always Keep", paste(keep, collapse=", ")))

message("All Features:")
message(paste("\t Score", score.min))

for (f in colnames(m)) {
    if (!f %in% keep) {
        exclude2 <- c(exclude, f)
        message(paste("Excluding:", paste(exclude2, collapse=", ")))
        
        score <- fuck(m[!colnames(m) %in% exclude2])
        message(paste("\tScore (current minimum):", score.min))
        message(paste("\tScore: ", score))
        if (score <= score.min) {
            score <- score.min
            exclude <- c(exclude, f)
            message(paste("Remove:", f))
            score.min <- score
        } else {
            message(paste("Don't Remove:", f))
        }
    }
}
