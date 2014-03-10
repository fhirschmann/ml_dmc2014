# Utility functions
suppressPackageStartupMessages(library(caret))

norm.lin <- function(x, mi=min(x), ma=max(x)) {
    (x - mi) / (ma - mi)
}

addlevels <- function (df1, df2) {
    # Adds factors levels from df2 to df1 and returns a new Data Frame.

    dfx <- df1
    facs <- names(sapply(df1, is.factor))
    for (fac in facs) {
        levels(dfx[[fac]]) <- c(levels(dfx[[fac]]), levels(df2[[fac]]))
    }
    dfx
}

table2 <- function(v1, v2, row.names=c("train", "test")) {
    # Frequency table for two factors with the same levels.
    #
    # Can be used for Chi-Sq test, e.g.:
    #   chisq.test(table2(dt$domain, dt.test$domain))
    #
    # Args:
    #   v1: first factor
    #   v2: second factor
    
    tbl <- rbind(table(v1), table(v2))
    rownames(tbl) <- row.names
    tbl
}

df.classes <- function(dt) {
    # Prints a Data Frame's column classes.
    #
    # Args:
    #   dt: a data frame
    
    df <- t(data.frame(lapply(dt, function(x) paste(class(x), collapse="/"))))
    colnames(df) <- c("class")
    df
}

df.nas <- function(dt) {
    # Prints the number of NAs.
    #
    # Args:
    #   dt: a data frame
    
    df <- t(data.frame(lapply(dt, function(x) sum(is.na(x)))))
    colnames(df) <- c("NAs")
    df
}

list.update <- function(l1, l2) {
    # Updates list l1 with the contents of l2 and returns
    # the new list.
    #
    # Args:
    #   l1: a list
    #   l2: a list
    
    lr <- l1[setdiff(names(l1), names(l2))]
    c(lr, l2)
}

caret.train.default.desc <- list(train.args=list(),
                                 data.fun=identity,
                                 serialize="models")

caret.train <- function(descs, common.desc=(d <- caret.train.default.desc),
                        verbose=FALSE) {
    # This is a wrapper around caret::train that allows
    # to pass a list of learner descriptions.
    #
    # Args:
    #   descs: learner descriptions
    #   common.desc: default description for all learners
    #   verbose: be verbose

    fits <- list()
    
    for (name in names(descs)) {
        message("Learning model for ", name)
        train.args <- list.update(common.desc$train.args, descs[[name]]$train.args)
        desc <- list.update(common.desc, descs[[name]])
        desc$train.args <- train.args
        desc$train.args$data <- desc$data.fun(desc$train.args$data)
        
        if (verbose) message(str(desc))
        
        # Train the model
        fit <- do.call(caret::train, train.args)
    
        # Serialize
        if (!is.null(desc$serialize)) {
            fname <- file.path(desc$serialize, paste(name, "RData", sep="."))
            #save(fit, file=fname)
            message("Wrote model to: ", fname)
        }
        fits[[name]] <- fit
    }
    
    fits
}

caret.load <- function(mdir="models") {
    # Returns a list of serialized models.
    #
    # Args:
    #   mdir: the directory to search for models in
    
    models <- list()
    cwd <- getwd()
    setwd(mdir)
    
    for (file in list.files(pattern=".RData")) {
        name <- unlist(strsplit(file, "\\."))[1]
        cat(paste("Loading model from", file, "\n"))
        load(file)
        models[[name]] <- fit
    }
    setwd(cwd)
    models
}

caret.bestidx <- function(fit) {
    # Returns the index of the best tune observations and predictions
    # for a caret fit. Don't think about what this function does too hard...
    #
    # The predictions are saved in fit$pred$pred, but this vector contains
    # the predictions for *all* models across *all* tuning parameters, so this
    # function returns a logical vector with the indices for the predictions
    # for the best tune set to TRUE.
    #
    # Args:
    #   dt: a caret fit
    
    params <- colnames(fit$bestTune)
    Reduce('&', lapply(params, function(y) fit$pred[[y]] == fit$bestTune[[y]]))
}

caret.pred <- function(fit) {
    # Extracts the predictions from a caret fit.
    #
    # Args:
    #   dt: a caret fit
    
    fit$pred$pred[caret.bestidx(fit)]
}

caret.obs <- function(fit) {
    # Extracts the observations from a caret fit.
    #
    # Args:
    #   dt: a caret fit

    fit$pred$obs[caret.bestidx(fit)]
}
