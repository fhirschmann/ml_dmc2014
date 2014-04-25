# DMC2014 Specific Stuff
source("R/feat.R")

DmcTrain <- function(method, data=dt.dmc, fs.fun=identity, ...) {
    models <- list()
    
    for (name in names(data)) {
        models[[name]] <- list()
        class(models[[name]]) <- "DmcFit"
        
        dt <- data[[name]]$train
        dt <- dt[!dt$deliveryDateMissing == "yes", ]
        models[[name]] <- list()
        models[[name]]$model <- method(returnShipment ~ ., data=fs.fun(dt), ...)
        models[[name]]$preds <- predict(models[[name]]$model, fs.fun(data[[name]]$test))
        models[[name]]$score <- dmc.points(models[[name]]$preds, data[[name]]$test$returnShipment)
    }

    results <- data.frame(cbind(
        sapply(models, function(x) x$score, USE.NAMES=F),
        sapply(dt.dmc, function(x) nrow(x$train), USE.NAMES=F),
        sapply(dt.dmc, function(x) nrow(x$test), USE.NAMES=F)))
    colnames(results) <- c("Score", "n_Train", "n_Test")
    results <- rbind(results, Total=colSums(results))
    results$Accuracy <- 1 - (results$Score / results$n_Test)
    
    res <- list(
      models=models,
      results=results
    )
    class(res) <- "DmcTrain"
    res
}

summary.DmcTrain <- function(train) {
    train$results
}

predict.DmcFit <- function(model, data) {
    dt <- data
    dt$returnShipment <- NULL

    dt$pred <- predict(model, dt)
    dt[dt$deliveryDateMissing == "yes", ]$pred <- "no"
    
    dt$pred
}

dmc.points <- function(pred, obs) {
    require(plyr)

    if (is.factor(pred)) {
        pred2 <- as.numeric(revalue(pred, c("no"="0", "yes"="1")))
    } else {
        pred2 <- as.numeric(pred)
    }
    
    if (is.factor(obs)) {
        obs2 <- as.numeric(revalue(obs, c("no"="0", "yes"="1")))
    } else {
        obs2 <- as.numeric(obs)
    }

    sum(abs(obs2 - pred2))
}

dmc.evaluate.model <- function(md) {
    sum(md$resample$Score)
}

# Summary function for caret
dmc.summary <- function(data, lev=NULL, model=NULL) {
    score <- dmc.points(data$pred, data$obs)
    c(Score=score)
}

dmc.inst <- function(upgrade=F) {
    # Installs the required dependencies.
    #
    # Args:
    #   upgrade: force upgrade
    
    libs <- c("devtools", "Metrics", "ellipse", "data.table", "knitr",
              "mda", "Hmisc", "lubridate", "earth", "pROC", "C50",
              "ROCR", "doMC", "caret", "chron", "RWeka", "klaR",
              "plyr", "MASS", "ggplot2", "ada", "earth", "kohonen",
              "functional")
    if (upgrade) {
        install.packages(libs)
    } else {
        install.packages(setdiff(libs, rownames(installed.packages())))
    }
}
