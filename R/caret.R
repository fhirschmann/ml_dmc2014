library(caret)
library(doMC)

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