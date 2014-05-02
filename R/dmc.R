# DMC2014 Specific Stuff
source("R/feat.R")
source("R/utils.R")

dmctrain <- function(dt.train, dt.test, fs.fun, method="rf",
                     trControl=trainControl(), ...) {
    ## Trains and tests on specific sets. Note that this
    ## simply concatenates the two sets, so the row indices of dt.test
    ## will by off by nrow(dt.train) in $preds.
    ##
    ## Args:
    ##   dt.train: train set
    ##   dt.test: test set
    ##   fs.fun: function applied to the data prior to learning
    ##   method: caret's method
    ##   trControl: caret's trainControl
    require(caret)
    
    data <- fs.fun(rbind(dt.train, dt.test))
    
    # Remove Zero-Variance Predictors; some algos can't handle them
    zeroVar <- names(which(sapply(dt.train, function(x) length(unique(x)) == 1)))
    message(paste("Excluding Zero Variance Predictors", paste(zeroVar, collapse=", ")))
    data <- data[!names(data) %in% zeroVar]
    
    message("Training on the following Data:")
    message(str(data))
    
    # Magic for train/test split
    trControl$index <- list(rs1=1:nrow(dt.train))
    trControl$indexOut <- list((rs1=nrow(dt.train)+1):nrow(data))
    trControl$method <- "cv"
    
    set.seed(42)
    fit <- caret::train(returnShipment ~ ., data=data, method=method,
                        trControl=trControl, na.action=na.pass, ...)

    fit
}

dmcmtrain <- function(data, fs.fun, method="rf", trControl=trainControl(), 
                      save.path=NULL, ...) {
    ## Trains and tests on a list of train and test sets.
    ##
    ## Args:
    ##   data: list(A=list(train=..., test=...), B=list(train=..., test=...))
    ##   fs.fun: function applied to the data prior to learning
    ##   method: caret's method
    ##   trControl: caret's trainControl
    ##   save.path: path to save the resulting models to
    require(foreach)
    require(plyr)
    
    models <- foreach(dt.name=names(data)) %do% {
        # We don't want to train on instances with missing deliv dates,
        # because they are practically unlabeled
        train.idx <- data[[dt.name]]$train$deliveryDateMissing == "no"
        test.idx <- data[[dt.name]]$test$deliveryDateMissing == "no"
        
        message(paste("Training", method, "on", dt.name))
        model <- dmctrain(data[[dt.name]]$train[train.idx, ], 
                          data[[dt.name]]$test[test.idx, ],
                          fs.fun, method, trControl, ...)
            
        # Copy the results from caret
        results <- model$results
        
        # Standard Deviation doesn't make sense, since we have one test set only
        results$scoreSD <- NULL
        
        # We divide by the full data set in order to take instances
        # with missing delivery dates into account.
        results$accuracy <- 1 - (results$score / nrow(data[[dt.name]]$test))
        
        # Copy the predictions from caret
        pred <- model$pred
        
        # The test set comes after the train set (in terms of row indices)
        pred$rowIndex <- pred$rowIndex - sum(train.idx)
        
        # Add orderItemID to pred
        map <- data.frame(rowIndex=1:sum(test.idx),
                          orderItemID=data[[dt.name]]$test[test.idx, ]$orderItemID)
    
        pred <- join(pred, map, by="rowIndex")
        
        list(model=model, results=results, pred=pred,
             skippedOrderItemID=as.numeric(as.character(data[[dt.name]]$test[!test.idx, ]$orderItemID)))
    }
    names(models) <- names(data)

    # Concatenate the results from all train/test set combinations
    results <- do.call(rbind, sapply(names(models),
                                     function(n) data.frame(models[[n]]$results, set=n),
                                     simplify=F))
    if (nrow(results) == length(models)) {
        # There is only one row in the tuning grid
        bestResults <- results
    } else {
        bestResults <- results[sapply(names(models),
                                      function(n) paste(n, rownames(models[[n]]$model$bestTune), sep=".")), ]
    }
    
    res <- list(models=models, results=results, bestResults=bestResults)
    class(res) <- "mtrain"
    
    if (!is.null(save.path)) {
        saveRDS(res, file=file.path(save.path, paste(method, "RData", sep=".")))
        saveRDS(res[c("results", "bestResults")],
                file=file.path(save.path, paste(method, "_res", ".RData", sep="")))
    }
    
    res
}

extractPreds.dmcmtrain <- function(mtrain) {
    ## Extracts the best predictions from multiple models
    ##
    ## Args:
    ##   data: a 'mtrain' object
    require(plyr)
    
    sapply(mtrain$models,
           function(x) {
               x$bestTune <- x$model$bestTune
               # Extract the predictions for the tuning parameters that
               # yielded the best results
               best <- caret.best(x)
               
               # Some instances were skipped due to missing delivery dates,
               # so we add them back and set them to "no"
               pred <- rbind(best[!is.na(best$obs), ][c("orderItemID", "pred")],
                             data.frame(orderItemID=x$skippedOrderItemID,
                                        pred=rep("no", length(x$skippedOrderItemID))))
               
               colnames(pred) <- c("orderItemID", "prediction")
               pred <- pred[order(pred$orderItemID), ]
               rownames(pred) <- NULL
               pred
           },
           simplify=F)
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
    
    do.call(dmcmtrain, args)
}

summary.dmcstrain <- function(object) {
    sapply(object, summary, simplify=F)
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

    sum(abs(obs2 - pred2), na.rm=T)
}

dmc.loadeva <- function(dir) {
    ## Loads the serialized evaluation results from a directory
    ## (for multiple learners)
    ##
    ## Args:
    ##   dir: directory to load from
    require(stringr)
    require(plyr)
    
    results <- sapply(list.files(dir, pattern="_res.RData", full.names=T),
                      readRDS, simplify=F)
    names(results) <- str_sub(list.files(dir, pattern="_res.RData"), 1, -11)
    
    comb <- do.call(rbind, sapply(names(results),
                                  function(x) data.frame(
                                      results[[x]]$bestResults[c("score", "accuracy", "set")],
                                      method=x),
                                  simplify=F))

    # Combine the accuracy for multiple learners across all set in a nicely
    # looking matrix
    comb.acc <- as.data.frame(matrix(nrow=length(levels(comb$method)),
                                     ncol=length(levels(comb$set))))
    colnames(comb.acc) <- levels(comb$set)
    rownames(comb.acc) <- levels(comb$method)
    
    for (m in rownames(comb.acc)) {
        for (s in colnames(comb.acc)) {
            acc <- comb[comb$set == s & comb$method == m, ]$accuracy
            comb.acc[m, s] <- if (length(acc) == 0) NA else acc
        }
    }
    
    # Wiki Markup for easier pasting
    wiki <- data.frame(apply(apply(round(comb.acc, 4), 2, paste), 1, function(x) paste(x, collapse="|")))
    rownames(wiki) <- rownames(comb.acc)
    colnames(wiki) <- "accuracy"
    wiki$accuracy <- paste("|R", wiki$accuracy, "|", sep="|")
        
    list(models=results, accuracy=comb.acc, wiki=wiki)
}

dmc.convertPreds <- function(preds) {
    require(plyr)
    
    as.character(revalue(preds, c("yes"="1", "no"="0")))
}

dmc.nzv <- function(dt, fs.fun=identity, ...) {
    ## Prints Zero and Near-Zero-Variance Statistics
    ##
    ## Args:
    ##   dt: a data frame
    ##   fs.fun: function to apply to the data frame
    sapply(dt, function(x) sapply(x, function(y) nearZeroVar(fs.fun(y), saveMetrics=T, ...),
                                  simplify=F),
           simplify=F)
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
