#!/usr/bin/env Rscript
# Usage: ./bin/fe.R c50 M10 [feature]
#   i.e. ./bin/fe.R c50 M10 itemID

source("R/data.R")
source("R/dmc.R")
source("R/fs.R")

args <- commandArgs(T)

a <- args[[1]] # c50
s <- args[[2]] # M10 M11
if (length(args) > 2) {
    only <- args[[3]]
} else {
    only <- NA
}

if (s %in% c("M10", "M20", "M30")) {
    fsx <- fs.noCustomer
    message("Removing Customer Feature")
} else {
    fsx <- identity
}
m <- dt.dmc[[s]]$train
m <- m[m$deliveryDateMissing == "no", ]

t <- dt.dmc[[s]]$test
t <- t[t$deliveryDateMissing == "no", ]

#m <- m[m$deliveryDateMissing == "no", ]
#m <- m[m$deliveryDateMissing == "no", ]

#m <- m[1:100, ]
#t <- t[1:10, ]

exclude <- c()
if (is.na(only)) {
    keep <- c("returnShipment", "size")    
} else {
    keep <- setdiff(colnames(m), only)
}
message(paste("Always keeping", paste(keep, collapse=", ")))

if (a == "c50") {
    m <- fsx(fs.tree(m))
    t <- fsx(fs.tree(t))
    t$deliveryDateMissing <- NULL
    t$deliveryDateMissing <- NULL
    require("C50")
    fuck <- function(dt) {
        fit <- C5.0(returnShipment ~ ., data=dt)
        preds <- predict(fit, t)
        dmc.score(preds, t$returnShipment)
    }
} else if (a == "gbm") {
    require("gbm")
    require("plyr")
    fuck <- function(dt) {
        fs <- Compose(fsx, fs.tree)
        new.test <- t[colnames(t) %in% colnames(dt)]
        fit <- dmctrain(data=list(train=dt, test=new.test), data.name = s, fs.fun = fs, verbose = F,
                       keep.data = F, method='gbm', tuneGrid=expand.grid(shrinkage=c(0.1), interaction.depth=1, n.trees=20))
        fit$bestResult$score
    }
}

score.min <- fuck(m)
message(paste("Always Keep", paste(keep, collapse=", ")))

message("All Features:")
message(paste("\t Score", score.min))

removed <- c()
kept <- c()

cols <- colnames(m)
for (f in sample(cols, length(cols))) {
    if (!f %in% keep) {
        exclude2 <- c(exclude, f)
        message(paste("Excluding:", paste(exclude2, collapse=", ")))
        
        score <- fuck(m[!colnames(m) %in% exclude2])
        message(paste("\tScore (current minimum):", score.min))
        message(paste("\tScore:", score))
        message(paste("\tChange:", score - score.min))
        if (score <= score.min) {
            exclude <- c(exclude, f)
            message(paste("Remove:", f))
            removed[[f]] <- score - score.min
            score.min <- score
        } else {
            message(paste("Don't Remove:", f))
            kept[[f]] <- score - score.min
        }
    }
}

message("")
message("Features Kept:")
data.frame(change=unlist(kept))
message("")
message("Features Removed")
data.frame(change=removed)
