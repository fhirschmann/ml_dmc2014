# DMC2014 Specific Stuff
source("R/feat.R")
source("R/utils.R")

dmctrain <- function(data, tuneGrid=NULL, fs.fun, verbose=T, method="nb",
                     save.path=NULL, ...) {
    require(caret)
    require(foreach)
    
    res <- list()
    
    if (is.null(tuneGrid)) {
        # TODO
    }
    
    res <- foreach(dt.name=names(data)) %do% {
        results <- tuneGrid
        models <- foreach(e=1:nrow(tuneGrid)) %do% {
            if (verbose)
                message(paste("Learning model for", method, "on", dt.name,
                              e, "/", nrow(tuneGrid)))
            fit <- caret::train(returnShipment ~ .,
                                data=fs.fun(data[[dt.name]]$train),
                                tuneGrid=tuneGrid[e, ],
                                trControl=trainControl(
                                    method="none"),
                                method=method,
                                ...)
            preds <- predict(fit, fs.fun(data[[dt.name]]$test), na.action=na.pass)
            score <- dmc.points(preds, data[[dt.name]]$test$returnShipment)
            list(fit=fit, score=score, accuracy=1 - (score / nrow(data[[dt.name]]$test)),
                 preds=preds, orderItemID=data[[dt.name]]$test$orderItemID)
        }
        results$score <- sapply(models, function(x) x$score)
        results$accuracy <- sapply(models, function(x) x$accuracy)
        list(models=models, results=results)
    }
    names(res) <- names(data)
    class(res) <- "dmctrain"
    
    if (!is.null(save.path)) {
        saveRDS(res, file=file.path(save.path, paste(method, "RData", sep=".")))
        write.csv(summary(res), file=file.path(save.path, paste(method, "csv", sep=".")))
    }
    
    res
}

summary.dmctrain <- function(object) {
    results <- sapply(names(object), function(n) data.frame(object[[n]]$results, set=n),
                      simplify=F)
    tbl <- do.call(rbind, results)
    rownames(tbl) <- NULL
    tbl
}

extractpreds.dmctrain <- function(dmctrain) {
    best <- sapply(dmctrain, function(x) which.min(x$results$score))
    preds <- lapply(names(dmctrain), function(x) dmctrain[[x]]$models[[best[[x]]]]$preds)
    
    
    preds <- do.call(c, sapply(dmctrain,
                               function(x) as.numeric(
                                   as.character(
                                       revalue(
                                           x$models[[which.min(x$results$score)]]$preds,
                                           c("no"="0", "yes"="1")))),
                               simplify=F))
    names(preds) <- NULL
    
    orderItemID <- do.call(c, sapply(dmctrain, function(x) as.character(
        x$models[[which.min(x$results$score)]]$orderItemID),
        simplify=F))
    names(orderItemID) <- NULL
    res <- data.frame(orderItemID=orderItemID, prediction=preds)
    res
}

dmcstrain <- function(descs, common.desc, train.only=NULL) {
    # Takes a list of learner descriptions and fits multiple models.
    #
    # Args:
    #   descs: learner description
    #   common.desc: description common to all learners
    #   train.only: train only a subset of learners
    #
    # Returns:
    #   a list of dmc.multifit objects
    
    fits <- list()
    
    for (name in (if (length(train.only) == 0) names(descs) else train.only)) {
        train.args <- list.update(common.desc$train.args, descs[[name]]$train.args)
        desc <- list.update(common.desc, descs[[name]])
        desc$train.args <- train.args
    
        fit <- do.call(dmctrain,
                       c(list(desc$data,
                         desc$tuneGrid,
                         desc$fs.fun,
                         desc$verbose,
                         desc$method,
                         desc$save.path),
                         desc$train.args))
        
        fits[[name]] <- fit
    }
    
    fits
}

summary.DmcTrain <- function(train) {
    train$results
}

exportPreds.DmcTrain <- function(train, path) {
    require(plyr)
    
    dir.create(path, recursive=T)
    for (name in names(train$models)) {
        preds <- revalue(train$models[[name]]$preds, c("yes"="1", "no"="0"))
        write.table(data.frame(orderItemID=train$models[[name]]$orderItemIDs,
                             prediction=preds),
                  sep=";", quote=F, row.names=F,
                  file=file.path(path, paste(name, "txt", sep=".")))
    }
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
