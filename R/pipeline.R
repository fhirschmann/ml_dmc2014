source("R/common.R")
dmc.libs()

# Support for custom metrics. Add any additional metrics you'd like
# to be computed to this function.
mysummary <- function(data, lev=NULL, model=NULL) {
    out <- c(mae(data$pred, data$obs),
            RMSE(data$pred, data$obs),
            R2(data$pred, data$obs))
    names(out) <- c("MAE", "RMSE", "R2")
    out
}

# Be deterministic
set.seed(42)

# Train control. We use 10-fold Cross-Validation and a custom summary
# function that provides a variety of metrics.
ctrl <- trainControl(method="cv", savePredictions=TRUE, summaryFunction=mysummary)

# RWeka doesn't do multiprocessing, so this disables multiprocessing when
# using any algorithms provided by Weka.
mytrain <- function(form, method, ...) {
    if (method %in% c("M5", "M5Rules")) {
        cat("Parallel Backend: Disabled\n")
        registerDoSEQ()
    } else {
        cat("Parallel Backend: Enabled\n")
        registerDoMC(2)
    }
    train(form, method=method, metric="MAE", maximize=F, trControl=ctrl, ...)
}

# The list of training functions
trainers <- list()

trainers$svmPoly <- function(form, ...) mytrain(form, method="svmPoly", tuneLength=8, ...)
trainers$lm <- function(form, ...) mytrain(form, method="lm", preProc=c("center", "scale"))

# To be continued
