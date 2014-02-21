### Common Options shared across scripts ###

options(
        stringAsFactor=T,
        repos=structure(c(CRAN="http://cran.r-mirror.de"))
)

dmc.libs <- function() {
    libs <- c("devtools",
              "Metrics",
              "ellipse",
              "pROC",
              "data.table",
              "knitr",
              "doMC",
              "caret",
              "RWeka",
              "klaR",
              "plyr",
              "MASS",
              "ggplot2")
    for (lib in libs) {
        library(lib, logical.return=TRUE,
                character.only=TRUE) || install.packages(lib)
    }
}

