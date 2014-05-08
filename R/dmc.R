# DMC2014 Specific Stuff
source("R/feat.R")
source("R/utils.R")
source("R/fs.R")
library(functional)


dmctrain <- function(data, data.name, fs.fun, name="unknown", trControl=trainControl(), 
                     save.path=NULL, save.model=FALSE, verbose=T, ...) {
    require(caret)
    
    if (verbose) message("Setting up Data")

    # Indices of the rows used for training
    train.idx <- data$train$deliveryDateMissing == "no"
    
    # Indices of the rows used for training
    test.idx <- data$test$deliveryDateMissing == "no"
    
    # Save orderItemID for later use
    orderItemID <- as.numeric(as.character(data$test$orderItemID))
    
    features.before <- colnames(data$train)
    
    data <- fs.all(rbind(data$train[train.idx, ], data$test[test.idx, ]))
    
    customer <- data.name %in% c("M11", "M21", "M31")
    
    if (!customer) {
        message("Removing Customer Features")
        data <- fs.noCustomer(data)
    }
    
    if (length(formals(fs.fun)) > 1) {
        data <- fs.fun(data, customer)
    } else {
        data <- fs.fun(data)
    }
        
    features.after <- colnames(data)
    
    if (verbose) {
        message(paste("Excluded through fs.fun were\n",
                      paste(setdiff(features.before, features.after), collapse="\n")),
                "")
    }
    
    # Remove Zero-Variance Predictors; some algos can't handle them
    zeroVar <- names(which(sapply(data, function(x) length(unique(x)) == 1)))
    if (verbose)
        message(paste("Excluding Zero Variance Predictors\n", paste(zeroVar, collapse="\n")),
                sep="")
    data <- data[!names(data) %in% zeroVar]
    
    if (verbose) {
        message("Training on the following Data:")
        message(str(data))
    }
    
    # Magic for train/test split
    trControl$index <- list(rs1=1:sum(train.idx))
    trControl$indexOut <- list(rs1=(sum(train.idx)+1):nrow(data))
    trControl$method <- "cv"
    trControl$savePredictions <- T
    trControl$summaryFunction <- function(data, lev=NULL, model=NULL) c(score=dmc.score(data$pred, data$obs))
        
    if (verbose) message("Starting to train model")
    
    set.seed(42)
    model <- caret::train(returnShipment ~ ., data=data,
                          trControl=trControl, na.action=na.pass, metric="score",
                          maximize=F, ...)
    
    if (verbose) message("Model trained. Calculating results")
    
    # Copy the results from caret
    results <- model$results
    
    # Standard Deviation doesn't make sense, since we have one test set only
    results$scoreSD <- NULL
    
    # We divide by the full data set in order to take instances
    # with missing delivery dates into account.
    results$accuracy <- 1 - (results$score / length(test.idx))
    
    if (nrow(results) == 1) {
        bestResults <- results
    } else {
        bestResults <- results[rownames(model$bestTune), ]
    }
    
    # Copy the predictions from caret
    pred <- model$pred
    
    # The test set comes after the train set (in terms of row indices)
    pred$rowIndex <- pred$rowIndex - sum(train.idx)
    
    # Add orderItemID to pred
    map <- data.frame(rowIndex=1:sum(test.idx), orderItemID=orderItemID[which(test.idx)])
    
    pred <- join(pred, map, by="rowIndex")
    
    res <- list(model=model, results=results, bestResults=bestResults, pred=pred,
                bestTune=model$bestTune, method=model$method,
                label=getModelInfo(model$method, regex=F)[[1]]$label,
                skippedOrderItemID=orderItemID[which(!test.idx)])
        
    if (!is.null(save.path)) {
        if (verbose) message("Results and Predictions")
        
        stem <- file.path(save.path, paste(name, data.name, sep="_"))
        saveRDS(res[c("results", "bestResults", "method", "label")],
                file=paste(stem, "_res.RData", sep=""))
        write.table(extractPreds.dmctrain(res), file=paste(stem, "_pred.txt", sep=""),
                    quote=F, row.names=F, sep=";")    
        
        if (save.model)    
            saveRDS(res, file=paste(stem, ".RData", sep=""))
    }
    
    class(res) <- "dmctrain"
    res
}

extractPreds.dmctrain <- function(train) {
    best <- caret.best(train)
    
    pred <- rbind(best[c("orderItemID", "pred")],
                  data.frame(orderItemID=train$skippedOrderItemID,
                             pred=rep("no", length(train$skippedOrderItemID))))
    pred$pred <- dmc.convertPreds(pred$pred)
    names(pred) <- c("orderItemID", "prediction")
    pred[order(pred$orderItemID), ]
}

dmcdtrain <- function(desc, common.desc) {
    ## Takes a learner description and fits a model
    ##
    ## Args:
    ##   descs: learner description
    ##   common.desc: description common to all learners
    ##
    ## Returns:
    ##   a mtrain object

    train.args <- list.update(common.desc$train.args, desc$train.args)
    desc <- list.update(common.desc, desc)
    desc$train.args <- NULL
    args <- c(desc, train.args)
    
    do.call(dmctrain, args)
}

dmc.score <- function(pred, obs, na.rm=F) {
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

    sum(abs(obs2 - pred2), na.rm=na.rm)
}

dmc.evaluate <- function(dir) {
    files <- list.files(path=dir, pattern=paste(pattern=".*_res.RData", sep=""))
    
    models <- sapply(files, function(x) readRDS(file.path(dir, x)), simplify=F)
    sets <- sapply(str_split(files, "_"), function(x) x[[2]])
    names <- sapply(str_split(files, "_"), function(x) x[[1]])
    
    i <- 0
    for (s in sets) {
        i <- i + 1;
        models[[i]]$set <- sets[[i]]
        models[[i]]$name <- names[[i]]
    }
    
    scores <- matrix(nrow=length(unique(names)), ncol=9)
    colnames(scores) <- c("M10", "M11", "M1", "M20", "M21", "M2", "M30", "M31", "M3")
    rownames(scores) <- unique(names)
    
    for (m in models) {
        scores[m$name, m$set] <- models[[1]]$bestResults$score
    }

    for (i in 1:3) {
        for (m in rownames(scores)) {
            scores[m, paste("M", i, sep="")] <- scores[m, paste("M", i, "0", sep="")] + scores[m, paste("M", i, "1", sep="")]
        }
    }
    
    # dput(sapply(dt.dmc, function(x) nrow(x$test)))
    nrows <- structure(c(69646L, 31007L, 38639L, 38586L, 14745L, 23841L, 38586L, 
                         12048L, 26538L), .Names = c("M1", "M10", "M11", "M2", "M20", 
                                                     "M21", "M3", "M30", "M31"))
    
    accuracies <- scores
    for (s in colnames(accuracies)) {
        for (m in rownames(accuracies)) {
            accuracies[m, s] <- 1 - (scores[m, s] / nrows[[s]])
        }
    }
    
    wiki <- data.frame(accuracies[, c("M1", "M2", "M3")])
    wiki <- data.frame(apply(accuracies[, c("M1", "M2", "M3")], 1,
                       function(x) paste("|R|", paste(x, collapse="|"), "|", sep="")))    
    colnames(wiki) <- "Markup"
    list(wiki=wiki, accuracies=accuracies, scores=scores)
}

dmc.evaluate2 <- function(dir) {
    require(caret)
    require(stringr)
    
    files <- list.files(path=dir, pattern=paste(pattern=".*_res.RData", sep=""))
    
    models <- sapply(files, function(x) readRDS(file.path(dir, x)), simplify=F)
    methods <- sapply(models, function(x) x$method)
    sets <- sapply(str_split(files, "_"), function(x) x[[2]])
    
    res <- do.call(rbind, mapply(function(model, method, set) {
        data.frame(method=method, set=set,
                   model$bestResults[c("score", "accuracy")])
    }, models, methods, sets, SIMPLIFY=F))
    res$label <- sapply(res$method, function(x) getModelInfo(x, regex=F)[[1]]$label)
    
    acc <- matrix(nrow=length(unique(methods)), ncol=7)
    rownames(acc) <- unique(methods)
    colnames(acc) <- c("T1", "T2", "T3", "X1", "X2", "X3", "C")
    
    for (m in rownames(acc)) {
        for (s in colnames(acc)) {
            acc.e <- res[res$set == s & res$method == m, ]$accuracy
            acc[m, s] <- if (length(acc.e) == 0) NA else as.numeric(acc.e)
        }
    }
    
    wiki <- data.frame(apply(apply(round(acc, 8), 2, paste), 1, function(x) paste(x, collapse="|")))
    rownames(wiki) <- NULL
    colnames(wiki) <- "accuracy"
    wiki$accuracy <- paste(paste("|R", sapply(res$method, function(x) getModelInfo(x, regex=F)[[1]]$label),
                                 wiki$accuracy, sep="|"), "|", sep="")
    
    list(results=res, accuracy=acc, wiki=wiki)
}

dmc.convertPreds <- function(preds) {
    require(plyr)
    
    as.character(revalue(preds, c("yes"="1", "no"="0")))
}

dmc.inst <- function(upgrade=F) {
    ## Installs the required dependencies.
    ##
    ## Args:
    ##   upgrade: force upgrade
    
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
