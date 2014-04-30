# DMC2014 Specific Stuff
source("R/feat.R")
source("R/utils.R")

dmctrain <- function(dt.train, dt.test, fs.fun, method="rf",
                     trControl=trainControl(), ...) {
    require(caret)
    
    data <- fs.fun(rbind(dt.train, dt.test))
    
    trControl$index <- list(rs1=1:nrow(dt.train))
    trControl$indexOut <- list(rs1=nrow(dt.train)+1:nrow(data))
    trControl$method <- "cv"
    
    caret::train(returnShipment ~ ., data=data, method=method,
                 trControl=trControl, na.action=na.pass, ...)
}

dmcmtrain <- function(data, fs.fun, method="rf", trControl=trainControl(), 
                      save.path=NULL, ...) {
    require(foreach)
    
    res <- foreach(dt.name=names(data)) %do% {
        dt.train <- data[[dt.name]]$train
        dt.train <- dt.train[dt.train$deliveryDateMissing == "no", ]
        
        dt.test <- data[[dt.name]]$test
        dt.test <- dt.test[dt.test$deliveryDateMissing == "no", ]
        
        message(paste("Training", method, "on", dt.name))
        model <- dmctrain(dt.train, dt.test, fs.fun, method, trControl, ...)
        list(model=model, orderItemID=dt.test$orderItemID)
    }
    names(res) <- names(data)
    
    class(res) <- "mtrain"
    
    if (!is.null(save.path)) {
        saveRDS(res, file=file.path(save.path, paste(method, "RData", sep=".")))
    }
    
    res
}

extractPreds.dmcmtrain <- function(mtrain) {
    
}

dmcdtrain <- function(desc, common.desc) {
    # Takes a learner description and fits a model
    #
    # Args:
    #   descs: learner description
    #   common.desc: description common to all learners
    #
    # Returns:
    #   a mtrain object

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
    }, simplify=F)
    comp <- join_all(comp, type="full")
    rownames(comp) <- names(best)

    list(models=results, comp=comp, best=best)
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
