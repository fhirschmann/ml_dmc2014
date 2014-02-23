### Common Options shared across scripts ###

options(
        stringAsFactor=T,
        repos=structure(c(CRAN="http://cran.r-mirror.de"))
)

dmc.inst <- function(upgrade=F) {
    libs <- c("devtools",
              "Metrics",
              "ellipse",
              "data.table",
              "knitr",
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

