options(
        stringAsFactor=F,
        repos=structure(c(CRAN="http://cran.r-mirror.de"))
)

.env <- new.env()

.env$dmc.inst <- function() {
    install.packages(c("caret", "Cubist", "RWeka", "earth",
                       "plyr", "party", "elasticnet", "evtree",
                       "C50", "extraTrees", "gam", "MASS",
                       "kernlab", "rpart", "kohonen", "lars",
                       "class", "nnet", "neuralnet", "ggplot2"))
}

attach(.env)
