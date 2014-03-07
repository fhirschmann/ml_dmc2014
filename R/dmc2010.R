# DMC2010 Specific Stuff
source("R/utils.R")

suppressPackageStartupMessages(library(caret))


dmc.points <- function(pred, obs) {
    voucher <- ifelse(pred == "yes", "no", "yes")
    sum(ifelse(voucher == "yes",
               ifelse(obs == "yes", -5, 1.5),
               0))
}

dmc.summary <- function(data, lev = NULL, model = NULL) {
    out <- twoClassSummary(data, lev, model)
    points <- dmc.points(data$pred, data$obs)
    maxpoints <- dmc.points(data$obs, data$obs)
    c(Points=points,
      PointsP=points / maxpoints,
      out)
}

dmc.evaluate <- function(mds) {
    df <- data.frame(lapply(mds, function(x) dmc.points(caret.pred(x), caret.obs(x))))
    df$baseline <- dmc.points(rep("no", length(caret.obs(mds[[1]]))), caret.obs(mds[[1]]))
    df$maximum <- dmc.points(caret.obs(mds[[1]]), caret.obs(mds[[1]]))
    rownames(df) <- c("Points")
    t(df)
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
