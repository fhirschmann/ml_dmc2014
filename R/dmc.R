# DMC2014 Specific Stuff


dmc.train <- function(method, data=dt.dmc, fs.fun=identity, ...) {
    fit <- list()
    
    for (name in names(data)) {
        dt <- data[[name]]$train
        dt <- dt[!dt$deliveryDateMissing == "yes", ]
        fit[[name]] <- method(returnShipment ~ ., data=fs.fun(dt), ...)
    }
    
    fit
}

dmc.evaluate <- function(fit, data=dt.dmc) {
    res <- list()

    for (name in names(data)) {
        res[[name]] <- dmc.points(dmc.predict(fit[[name]], data[[name]]$test), data[[name]]$test$returnShipment)
    }
    
    res
}

dmc.predict <- function(model, data) {
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
