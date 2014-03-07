# Utility functions

as.binary <- function(x) {
    if (length(unique(x)) != 2) {
        stop("The given variable is not binary.")
    }
    as.factor(ifelse(x == 1, "yes", "no"))
}

df.classes <- function(dt) {
    df <- t(data.frame(lapply(dt, function(x) paste(class(x), collapse="/"))))
    colnames(df) <- c("class")
    df
}

df.nas <- function(dt) {
    df <- t(data.frame(lapply(dt, function(x) sum(is.na(x)))))
    colnames(df) <- c("NAs")
    df
}

caret.bestidx <- function(fit) {
    params <- colnames(fit$bestTune)
    Reduce('&', lapply(params, function(y) fit$pred[[y]] == fit$bestTune[[y]]))
}

caret.pred <- function(fit) {
    fit$pred$pred[caret.bestidx(fit)]
}

caret.obs <- function(fit) {
    fit$pred$obs[caret.bestidx(fit)]
}

dmc.inst <- function(upgrade=F) {
    libs <- c("devtools",
              "Metrics",
              "ellipse",
              "data.table",
              "knitr",
              "mda",
              "Hmisc",
              "lubridate",
              "earth",
              "pROC",
              "C50",
              "ROCR",
              "doMC",
              "caret",
              "chron",
              "RWeka",
              "klaR",
              "plyr",
              "MASS",
              "ggplot2")
    if (upgrade) {
        install.packages(libs)
    } else {
        install.packages(setdiff(libs, rownames(installed.packages())))
    }
}

dmc.load <- function(mdir="models") {
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
    fit <- trainers[[name]]()
    
    fname <- file.path("models", paste(name, ".RData", sep=""))
    save(fit, file=fname)
    
    cat("Wrote model for", name, "to", fname, "\n")
    fit
}
