# Utility functions
suppressPackageStartupMessages(library(caret))

as.binary <- function(x, cond=1) {
    # Makes an observation pseudo-binary, by assigning "yes" to the
    # observation that meets the given requirement, and "no" otherwise.
    # This is useful where logicals are not supported.
    #
    # Args:
    #   x: a vector
    #   cond: the condition to meet
    #
    # Returns:
    #   a pseudo-binary factor
    
    if (length(unique(x)) != 2) stop("The given variable is not binary.")
    x2 <- as.factor(ifelse(x == cond, "yes", "no"))
    if (length(unique(x2)) != 2) stop("The resulting variable is not binary")
    x2
}

chisq.test2 <- function(v1, v2, ...) {
    chisq.test(table(v1), p=prop.table(table(v2)))
}

table2 <- function(v1, v2, col.names=NULL) {
    t1 <- table(v1)
    t2 <- table(v2)
    m <- matrix(c(t1, t2), 2, byrow=T)
    colnames(m) <- levels(v1)
    if (!is.null(row.names)) rownames(m) <- col.names
    t(m)
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

caret.train <- function(descs, train.args=list(), serialize="models", verbose=F) {
    # This is a wrapper around caret::train that allows
    # to pass a list of learner descriptions.
    #
    # Args:
    #   descs: learner descriptions
    #   serialize: directory to serialize models to
    #   train.args: common arguments to train used for all learners
    fits = list()
    
    for (name in names(descs)) {
        message("Learning model for ", name)
        desc <- descs[[name]]

        train.args <- c(train.args[setdiff(names(train.args),
                                           names(desc$train.args))],
                        desc$train.args)
        if (verbose) {
            message("Calling caret::train")
            message(str(train.args))
        }
        
        # Train the model
        fit <- do.call(caret::train, train.args)
    
        # Serialize
        if (!is.null(serialize)) {
            fname <- file.path(serialize, paste(name, "RData", sep="."))
            save(fit, file=fname)
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
