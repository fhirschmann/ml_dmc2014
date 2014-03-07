# Utility functions

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
    
    if (length(unique(x)) != 2) {
        stop("The given variable is not binary.")
    }
    as.factor(ifelse(x == 1, "yes", "no"))
}

df.classes <- function(dt) {
    # Prints a Data Frame's column classes
    #
    # Args:
    #   dt: a data frame
    
    df <- t(data.frame(lapply(dt, function(x) paste(class(x), collapse="/"))))
    colnames(df) <- c("class")
    df
}

df.nas <- function(dt) {
    # Prints the number of NAs
    #
    # Args:
    #   dt: a data frame
    
    df <- t(data.frame(lapply(dt, function(x) sum(is.na(x)))))
    colnames(df) <- c("NAs")
    df
}

caret.bestidx <- function(fit) {
    # Returns the index of the best tune observations and predictions
    # for a caret fit.
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

dmc.inst <- function(upgrade=F) {
    # Installs the required dependencies.
    #
    # Args:
    #   upgrade: force upgrade

    libs <- c("devtools", "Metrics", "ellipse", "data.table", "knitr",
              "mda", "Hmisc", "lubridate", "earth", "pROC", "C50",
              "ROCR", "doMC", "caret", "chron", "RWeka", "klaR",
              "plyr", "MASS", "ggplot2")
    if (upgrade) {
        install.packages(libs)
    } else {
        install.packages(setdiff(libs, rownames(installed.packages())))
    }
}

dmc.load <- function(mdir="models") {
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

dmc.run <- function(name) {
    # Runs training for an algorithm and serializes the
    # fitted model.
    #
    # Args:
    #   name: the name of the training function

    fit <- trainers[[name]]()
    
    fname <- file.path("models", paste(name, ".RData", sep=""))
    save(fit, file=fname)
    
    cat("Wrote model for", name, "to", fname, "\n")
    fit
}
