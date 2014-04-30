# DMC2014 Specific Stuff
source("R/feat.R")
source("R/utils.R")

dmctrain <- function(data, tuneGrid=NULL,
                     preProcess = NULL,
                     tuneLength=3, fs.fun, verbose=T, method="nb",
                     save.path=NULL, ...) {
    require(caret)
    require(foreach)
    
    res <- list()
    models <- getModelInfo(method, regex=FALSE)[[1]]
    
    ## Gather all the pre-processing info. We will need it to pass into the grid creation
    ## code so that there is a concorance between the data used for modeling and grid creation
    if(!is.null(preProcess))
    {
        ppOpt <- list(options = preProcess)
        if(length(trControl$preProcOptions) > 0) ppOpt <- c(ppOpt,trControl$preProcOptions)
    } else ppOpt <- NULL
    
    ## If no default training grid is specified, get one. We have to pass in the formula
    ## and data for some models (rpart, pam, etc - see manual for more details)
    if(is.null(tuneGrid)) {
        if(!is.null(ppOpt) && length(models$parameters$parameter) > 1 && as.character(models$parameters$parameter) != "parameter") {
            # Haxx
            x <- data$T1$train
            x$returnShipment <- NULL
            y <- data$T1$returnShipment
            
            pp <- list(method = ppOpt$options)
            if("ica" %in% pp$method) pp$n.comp <- ppOpt$ICAcomp
            if("pca" %in% pp$method) pp$thresh <- ppOpt$thresh
            if("knnImpute" %in% pp$method) pp$k <- ppOpt$k   
            pp$x <- x
            ppObj <- do.call("preProcess", pp)
            tuneGrid <- models$grid(predict(ppObj, x), y, tuneLength)
            rm(ppObj, pp)
        } else tuneGrid <- models$grid(x, y, tuneLength)
    }
    if (verbose) {
        message("Tuning Grid:")
        print(tuneGrid)
    }
    
    res <- foreach(dt.name=names(data)) %do% {
        results <- tuneGrid
        models <- foreach(e=1:nrow(tuneGrid), .packages=c("caret", "ada")) %dopar% {
            if (verbose)
                message(paste("Learning model for", method, "on", dt.name,
                              e, "/", nrow(tuneGrid)))
            dt.train <- data[[dt.name]]$train
            dt.train <- dt.train[dt.train$deliveryDateMissing == "no", ]
            dt.test <- data[[dt.name]]$test
            
            dt.train.fs <- fs.fun(dt.train)
            dt.test.fs <- fs.fun(dt.test)
            
            # If we want to predict later, dt.train needs to have all the
            # levels that are in dt.test
            dt.train.fs.lvl <- addlevels(dt.train.fs, dt.test.fs)
            dt.test.fs.lvl <- addlevels(dt.test.fs, dt.train.fs)
            
            fit <- caret::train(returnShipment ~ .,
                                data=dt.train.fs.lvl,
                                tuneGrid=tuneGrid[e, ],
                                trControl=trainControl(method="none"),
                                method=method,
                                ...)
            
            dt.test$pred <- predict(fit, dt.test.fs.lvl, na.action=na.pass)
            dt.test[dt.test$deliveryDateMissing == "yes", ]$pred <- "no"
            score <- dmc.points(dt.test$pred, data[[dt.name]]$test$returnShipment)
            list(fit=fit, score=score, accuracy=1 - (score / nrow(dt.test)),
                 pred=dt.test$pred, orderItemID=data[[dt.name]]$test$orderItemID)
        }
        results$score <- sapply(models, function(x) x$score)
        results$accuracy <- sapply(models, function(x) x$accuracy)
        list(models=models, results=results)
    }
    names(res) <- names(data)
    class(res) <- "dmctrain"
    
    if (!is.null(save.path)) {
        saveRDS(res, file=file.path(save.path, paste(method, "RData", sep=".")))
        write.csv(summary(res), file=file.path(save.path, paste(method, "csv", sep=".")),
                  quote=F, row.names=F)
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

extractPreds.dmctrain <- function(dmctrain) {
    require(plyr)
    
    best <- sapply(dmctrain, function(x) which.min(x$results$score))
    preds <- lapply(names(dmctrain), function(x) dmctrain[[x]]$models[[best[[x]]]]$pred)
    
    
    preds <- do.call(c, sapply(dmctrain,
                               function(x) as.numeric(
                                   as.character(
                                       revalue(
                                           x$models[[which.min(x$results$score)]]$pred,
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
                         desc$preProcess,
                         desc$tuneLength,
                         desc$fs.fun,
                         desc$verbose,
                         desc$method,
                         desc$save.path),
                         desc$train.args))
        
        fits[[name]] <- fit
    }
    
    class(fits) <- "dmcstrain"
    
    fits
}

summary.dmcstrain <- function(object) {
    sapply(object, summary, simplify=F)
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

dmc.evaluate <- function(dir) {
    require(stringr)
    require(plyr)
    
    results <- sapply(list.files(dir, pattern=".csv", full.names=T),
                      read.csv, simplify=F)
    names(results) <- str_sub(list.files(dir, pattern=".csv"), 1, -5)
    
    best <- sapply(results,
                   function(m) ddply(m, .(set), function(x) x[which.min(x$score), ]),
                   simplify=F)

    comp <- sapply(best, function(b) {
        df <- t(data.frame(b$accuracy))
        colnames(df) <- b$set
        rownames(df) <- NULL
        data.frame(df)
    })
    comp <- join_all(comp, type="full")
    rownames(comp) <- names(best)

    sets <- unique(do.call(c, sapply(best, function(x) levels(x$set))))
    list(models=results, comp=comp, best=best)
}

dmc.grid <- function(method, dt, tuneLength=3) {
    require(caret)
    getModelInfo(method)[[method]]$grid(dt, dt$returnShipment, tuneLength)
}

dmc.nzv <- function(dt, fs.fun, ...) {
    sapply(dt, function(x) sapply(x, function(y) nearZeroVar(fs.fun(y), saveMetrics=T, ...),
                                  simplify=F),
           simplify=F)
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
