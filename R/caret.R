library(caret)
library(doMC)
library(ROCR)

# RWeka doesn't do multiprocessing, so this disables multiprocessing when
# using any algorithms provided by Weka.
train <- function(form, method=method, ...) {
    if (method %in% c("M5", "M5Rules")) {
        cat("Parallel Backend: Disabled\n")
        #registerDoSEQ()
    } else {
        cat("Parallel Backend: Enabled\n")
        #registerDoMC(2)
    }
    caret::train(form, method=method, ...)
}

# Support for custom metrics. Add any additional metrics you'd like
# to be computed to this function.
#
# Just here for reference -- this is for regression
mysummary <- function(data, lev=NULL, model=NULL) {
    out <- c(mae(data$pred, data$obs),
             RMSE(data$pred, data$obs),
             R2(data$pred, data$obs))
    names(out) <- c("MAE", "RMSE", "R2")
    out
}