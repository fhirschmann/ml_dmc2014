suppressMessages(library(caret))


dmc.points <- function(dt) {
    ifelse(dt$target90 == "no", 
           ifelse(dt$voucher == "no", 0, 1.5), 
           ifelse(dt$voucher == "no", 1.5, -5))
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