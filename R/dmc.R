# DMC2014 Specific Stuff
source("R/feat.R")
source("R/utils.R")

dmc.train <- function(data, tuneGrid=NULL, fs.fun, verbose=T, method="nb", ...) {
    require(caret)
    
    res <- list()
    class(res) <- "dmctrain"
    
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
            list(fit=fit, score=score, accuracy=1 - (score / nrow(data[[dt.name]]$test)), preds=preds)
        }
        results$score <- sapply(models, function(x) x$score)
        results$accuracy <- sapply(models, function(x) x$accuracy)
        list(models=models, results=results)
    }
    names(res) <- names(data)
    
    res
}

summary.dmctrain <- function(dmctrain) {
    results <- sapply(names(dmctrain), function(n) data.frame(dmctrain[[n]]$results, set=n),
                      simplify=F)
    tbl <- do.call(rbind, results)
    rownames(tbl) <- NULL
    tbl
}


dmc.multitrain <- function(form, data, method, fs.fun=identity,
                           trControl=trainControl(), save.path=NA, 
                           verbose=F, ...) {
    # Trains a model using multiple train/test splits.
    # Args:
    #   form: formula
    #   data: data, list(T1=list(train=dt.test, test=dt.test))
    #   method: method for caret::train
    #   fs.fun: feature selection function
    #   trControl: caret::trainControl
    #   save.path: filename stem to save to
    #   ...: passed to dmc.train
    #
    # Returns:
    #   a dmc.multitrain object
    
    require(foreach)
    
    models <- foreach(dt.name=names(data)) %do% {
        message(paste("Learning model", method, "on", dt.name))
        dmc.train(form=form, 
                  dt.train=data[[dt.name]]$train,
                  dt.test=data[[dt.name]]$test,
                  trControl=trControl,
                  save.path=save.path,
                  method=method,
                  save.fname=paste(dt.name, method, sep="_"),
                  verbose=verbose,
                  ...)
    }
    names(models) <- names(dt.dmc)
    
    res <- list(models=models)
    class(res) <- "dmc.multitrain"
    res
}

dmc.supertrain.default.desc <- list(train.args=list(),
                                    data.fun=identity,
                                    save.path="models")

dmc.supertrain <- function(descs, common.desc=(d <- caret.train.default.desc),
                           train.only=NULL) {
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
    
        fit <- do.call(dmc.multitrain, c(desc$train.args,
                                         fs.fun=desc$fs.fun,
                                         trControl=desc$train.args$trControl,
                                         save.path=desc$save.path,
                                         verbose=desc$verbose))
        
        fits[[name]] <- fit
    }
    
    fits
}

DmcTrain <- function(method, data=dt.dmc, fs.fun=identity, ...) {
    models <- list()
    
    for (name in names(data)) {
        models[[name]] <- list()
        class(models[[name]]) <- "DmcFit"
        
        dt <- data[[name]]$train
        dt <- dt[!dt$deliveryDateMissing == "yes", ]
        models[[name]] <- list()
        models[[name]]$model <- method(returnShipment ~ ., data=fs.fun(dt), ...)
        models[[name]]$orderItemIDs <- data[[name]]$test$orderItemID
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
