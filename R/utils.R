# Utility functions

as.binary <- function(x) {
    if (length(unique(x)) != 2) {
        stop("The given variable is not binary.")
    }
    as.factor(ifelse(x == 1, "yes", "no"))
}

dmc.inst <- function(upgrade=F) {
    libs <- c("devtools",
              "Metrics",
              "ellipse",
              "data.table",
              "knitr",
              "mda",
              "earth",
              "C50",
              "ROCR",
              "doMC",
              "caret",
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
