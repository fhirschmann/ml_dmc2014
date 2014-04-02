# DMC2014 Specific Stuff

dmc.points <- function(pred, obs) {
    sum(abs(obs - pred))
}

# Summary function for caret
dmc.summary <- function(data, lev=NULL, model=NULL) {
    score <- dmc.points(data$pred, data$obs)
    c(Score=score)
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
