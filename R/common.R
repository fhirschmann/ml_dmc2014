options(
        stringAsFactor=T,
        repos=structure(c(CRAN="http://cran.r-mirror.de"))
)

dmc.libs <- function() {
    libs <- c("devtools",
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
    library("Metrics", 
            logical.return=TRUE) || install_github("Metrics", "fhirschmann", subdir="R")
}

