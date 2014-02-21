library(caret)
library(doMC)

# RWeka doesn't do multiprocessing, so this disables multiprocessing when
# using any algorithms provided by Weka.
train <- function(form, ...) {
    if (method %in% c("M5", "M5Rules")) {
        cat("Parallel Backend: Disabled\n")
        registerDoSEQ()
    } else {
        cat("Parallel Backend: Enabled\n")
        registerDoMC(2)
    }
    caret::train(form, ...)
}

# Support for custom metrics. Add any additional metrics you'd like
# to be computed to this function.
mysummary <- function(data, lev=NULL, model=NULL) {
    out <- c(mae(data$pred, data$obs),
             RMSE(data$pred, data$obs),
             R2(data$pred, data$obs))
    names(out) <- c("MAE", "RMSE", "R2")
    out
}