# Training function definition
source("R/dmc.R")
source("R/fs.R")
source("R/feat.R")

library(functional)
library(caret)
library(C50)

ctrl <- trainControl(savePredictions=T, returnData=T)

# Don't pass this to classifiers who do not return class probabilities
ctrl.probs <- ctrl
ctrl.probs$classProbs <- TRUE

# List of stuff to learn
descs <- list(
    nb=list(
        method="nb",
        train.args=list()
    ),
    c50=list(
        fs.fun=fs.tree,
        method="C5.0",
        tuneGrid=expand.grid(
            .winnow=c(FALSE),
            .model=c("rules", "tree"),
            .trials=1:2
        ),
        train.args=list(
            na.action=na.pass,
            control=C5.0Control(earlyStopping=F))
    )
)

# Common description
common.desc <- list(
    # Be verbose
    verbose=T,
    # Serialize models to models/ directory
    save.path="models",
    # Function to apply to the data frame
    fs.fun=fs.all,
    # trainControl
    trControl=ctrl,
    # Data
    data=dt.dmc.mini
)
