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


dmc.tunecut <- function(fit, steps=1:40 * 0.025, return.best=F) {
    res <- sapply(steps,
                  function(x) dmc.points(
                      as.factor(with(caret.prob(fit, fix.nas=c(0.81, 0.19)), ifelse(no > x, "no", "yes"))),
                      caret.obs(fit)))
    names(res) <- steps
    if (return.best) {
        as.numeric(names(which(res == max(res))))
    } else {
        res
    }
}

dmc.ensemble.pred <- function(rf.probs, c50.preds) {
    as.factor(ifelse(rf.probs$no > 0.9999 | c50.preds == "no", "no", "yes"))
}

dmc.evaluate <- function(mds) {
    if (length(unique(lapply(mds, function(x) length(caret.pred(x))))) != 1)
        stop("Predictions are not of the same length. Different data sets?")
    df <- data.frame(lapply(mds, function(x) dmc.points(caret.pred(x), caret.obs(x))))

    # Probabilistic methods
    p <- sapply(mds, function(x) x$control$classProbs)
    tuned <- as.data.frame(lapply(mds[p], function(x) dmc.points(
        as.factor(ifelse(caret.prob(x, fix.nas=c(0.2, 0.8))$no > dmc.tunecut(x, return.best=T), "no", "yes")),
        caret.obs(x))))
    
    colnames(tuned) <- paste(colnames(tuned), "t", sep="_")
    rownames(df) <- rownames(tuned) <- c("Points")
    df <- rbind(data.frame(t(df)), data.frame(t(tuned)))
    df[order(df$Points, decreasing=T), , drop=F]
}

dmc.reload <- function() {
    source("R/dmc.R")
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
              "plyr", "MASS", "ggplot2", "ada", "earth", "kohonen",
              "functional")
    if (upgrade) {
        install.packages(libs)
    } else {
        install.packages(setdiff(libs, rownames(installed.packages())))
    }
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
