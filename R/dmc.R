# DMC2010 Specific Stuff
source("R/utils.R")

suppressPackageStartupMessages(library(caret))


dmc.points.each <- function(pred, obs) {
    voucher <- ifelse(pred == "yes", "no", "yes")
    ifelse(voucher == "yes",
           ifelse(obs == "yes", -5, 1.5),
           0)
}

dmc.points <- function(pred, obs) {
    sum(dmc.points.each(pred, obs))
}

dmc.summary <- function(data, lev = NULL, model = NULL) {
    points <- dmc.points(data$pred, data$obs)
    maxpoints <- dmc.points(data$obs, data$obs)
    c(Points=points,
      PointsRatio=points / maxpoints)
}


dmc.tunecut <- function(fit, steps=1:40 * 0.025) {
    res <- sapply(steps,
                  function(x) dmc.points(
                      as.factor(with(caret.prob(fit), ifelse(no > x, "no", "yes"))),
                      caret.obs(fit)))
    names(res) <- steps
    res    
}

dmc.evaluate <- function(mds) {
    if (length(unique(lapply(mds, function(x) length(caret.pred(x))))) != 1)
        stop("Predictions are not of the same length. Different data sets?")
    df <- data.frame(lapply(mds, function(x) dmc.points(caret.pred(x), caret.obs(x))))
    df$baseline <- dmc.points(rep("no", length(caret.obs(mds[[1]]))), caret.obs(mds[[1]]))
    df$maximum <- dmc.points(caret.obs(mds[[1]]), caret.obs(mds[[1]]))
    rownames(df) <- c("Points")
    t(df)
}

dmc.reload <- function() {
    rm(list=ls())
    source("R/dmc.R")
    source("R/common.R")
    source("R/data.R")
    source("R/utils.R")
    source("R/pipeline.R")
}

dmc.inst <- function(upgrade=F) {
    # Installs the required dependencies.
    #
    # Args:
    #   upgrade: force upgrade
    
    libs <- c("devtools", "Metrics", "ellipse", "data.table", "knitr",
              "mda", "Hmisc", "lubridate", "earth", "pROC", "C50",
              "ROCR", "doMC", "caret", "chron", "RWeka", "klaR",
              "plyr", "MASS", "ggplot2", "ada", "earth", "kohonen",)
    if (upgrade) {
        install.packages(libs)
    } else {
        install.packages(setdiff(libs, rownames(installed.packages())))
    }
}

dmc.run <- function(name) {
    # Runs training for an algorithm and serializes the
    # fitted model.
    #
    # Args:
    #   name: the name of the training function
    
    source("R/pipeline.R")
    fits <- caret.train(descs[name], common.desc, verbose=T)
    fits[[name]]
}

#Kostenmatrix:
#ursprünglich:
#			obs=1	obs=0
#voucher=1	-5		1.5
#voucher=0	0		0
#
#da voucher = !target90 ergibt sich
#
#			obs=1	obs=0
#target90=1	0		0
#target90=0	-5		1.5
#
#umformen für kostenmatrix (matrix will minimieren, nicht maximieren) ergibt
#
#			obs=1	obs=0
#target90=1	0		0
#target90=0	5		-1.5
#
#kostenmatrix muss auf der diagonalen 0 sein, ergibt:
#
#			obs=1	obs=0
#target90=1	0		1.5
#target90=0	5		0
dmc.cost <- matrix(c(0, 1.5, 5, 0), 2, 2, byrow=TRUE)
colnames(dmc.cost) <- rownames(dmc.cost) <- rev(c("no", "yes"))
