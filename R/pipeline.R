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
        fs.fun=fs.nb,
        method="nb"
    ),
    rf=list(
        fs.fun=fs.rf,
        method="rf",
        tuneLength=2
    ),
    c50=list(
        fs.fun=fs.tree,
        method="C5.0",
        #tuneGrid=expand.grid(
        #    trials=1,
        #    model="rules",
        #    winnow=F
        #),
        tuneLength=6,
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
    # Tune Length
    tuneLength=3,
    # trainControl
    trControl=ctrl,
    preProcess=NULL,
    # Data
    data=dt.dmc.mini
)
