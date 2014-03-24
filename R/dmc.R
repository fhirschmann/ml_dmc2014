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

dmc.ensemble <- function(preds) {
    as.factor(ifelse(Reduce("+",lapply(preds[c("rf_t")],
                                       function(x) ifelse(x == "yes", 1, -1))) > 0,
                     "yes", "no"))
}

dmc.evaluate <- function(preds, actual) {
    if(!all(lapply(preds, length) == length(actual)))
        stop("Predictions are not of the same length. Different data sets?")
    df <- data.frame(lapply(mds, function(x) dmc.points(caret.pred(x), caret.obs(x))))

    res <- data.frame(t(data.frame(lapply(preds, function(x) dmc.points(x, actual)))))
    colnames(res) <- "Points"
    
    res[order(res$Points, decreasing=T), , drop=F]
}

dmc.evaluate.train <- function() {
    preds <- lapply(mds, caret.pred)
    probs <- lapply(mds[c("rf", "nb")], function(x) caret.prob(x, fix.nas=c(0.8, 0.2)))
    preds[c("rf_t", "nb_t")] <- dmc.tune(probs)
    dmc.evaluate(preds, dt$target90)
}

dmc.evaluate.test <- function() {
    require(plyr)
    
    real <- read.csv("task2010/dmc2010_real.txt", sep=";")
    colnames(real) <- c("customernumber", "target90")
    real$target90 <- revalue(as.factor(real$target90), c("1"="yes", "0"="no"))
    m <- join(dt.test, real, by="customernumber")
    
    # There are more instances in "real" than there are in the test set...
    m[is.na(m$target90),]$target90 <- "yes"
    
    preds <- dmc.predict(mds, m)
    dmc.evaluate(preds, m$target90)
}

dmc.tune <- function(probs, cuts=list(nb=0.675, rf=0.65)) {
    lapply(names(probs), function(x) as.factor(ifelse(probs[[x]]$no > cuts[[x]], "no", "yes")))
}

dmc.predict <- function(mds, newdata, cuts=list(nb=0.675, rf=0.65)) {
    require(kohonen)
    require(klaR)
    require(C50)
    require(randomForest)
    require(elmNN)
    require(MASS)
    library(nnet)
    
    preds <- list()
    
    for (name in names(mds)) {
        if (name %in% names(cuts)) {
            p <- predict(mds[[name]], newdata, type="prob")
            preds[[paste(name, "t", sep="_")]] <- as.factor(ifelse(
                is.na(p$no) | p$no > cuts[[name]], "no", "yes"))
        }
        preds[[name]] <- predict(mds[[name]], newdata)
    }

    preds    
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
