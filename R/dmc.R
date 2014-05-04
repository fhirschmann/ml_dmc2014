# DMC2014 Specific Stuff
source("R/feat.R")
source("R/utils.R")


dmctrain <- function(data, data.name, fs.fun, method="rf", trControl=trainControl(), 
                     save.path=NULL, ...) {
    require(caret)

    # Indices of the rows used for training
    train.idx <- data$train$deliveryDateMissing == "no"
    
    # Indices of the rows used for training
    test.idx <- data$test$deliveryDateMissing == "no"
    
    # Save orderItemID for later use
    orderItemID <- as.numeric(as.character(data$test$orderItemID))
    
    data <- fs.fun(rbind(data$train[train.idx, ], data$test[test.idx, ]))
    
    # Remove Zero-Variance Predictors; some algos can't handle them
    zeroVar <- names(which(sapply(data, function(x) length(unique(x)) == 1)))
    message(paste("Excluding Zero Variance Predictors", paste(zeroVar, collapse=", ")))
    data <- data[!names(data) %in% zeroVar]
    
    message("Training on the following Data:")
    message(str(data))
    
    # Magic for train/test split
    trControl$index <- list(rs1=1:sum(train.idx))
    trControl$indexOut <- list(rs1=(sum(train.idx)+1):nrow(data))
    trControl$method <- "cv"
    trControl$savePredictions <- T
    trControl$summaryFunction <- function(data, lev=NULL, model=NULL) c(score=dmc.score(data$pred, data$obs))
        
    set.seed(42)
    model <- caret::train(returnShipment ~ ., data=data, method=method,
                          trControl=trControl, na.action=na.pass, metric="score",
                          maximize=F, ...)
    
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
                bestTune=model$bestTune,
                skippedOrderItemID=orderItemID[which(!test.idx)])
        
    if (!is.null(save.path)) {
        stem <- file.path(save.path, paste(method, data.name, sep="_"))
        saveRDS(res, file=paste(stem, ".RData", sep=""))
        saveRDS(res[c("results", "bestResults")],
                file=paste(stem, "_res.RData", sep=""))
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
