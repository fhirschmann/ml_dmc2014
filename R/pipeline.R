# Training function definition
source("R/dmc.R")
source("R/fs.R")
source("R/feat.R")

library(functional)
library(caret)
library(C50)

ctrl <- trainControl(savePredictions=T, returnData=T,
                     summaryFunction=function(data, lev=NULL, model=NULL) {
                         c(score=dmc.score(data$pred, data$obs, na.rm=T)) })

# Don't pass this to classifiers who do not return class probabilities
ctrl.probs <- ctrl
ctrl.probs$classProbs <- TRUE

# List of stuff to learn
descs <- list(
    nb=list(
        fs.fun=fs.nb,
        train.args=list(
            method="nb"
        )
    ),
    rf=list(
        fs.fun=fs.rf,
        train.args=list(
            method="rf",
            tuneLength=2
        )
    ),
    rf2=list(
        fs.fun=fs.rf,
        train.args=list(
            method="extraTrees",
            tuneLength=1
        )
    ),
    orf=list(
        fs.fun=fs.rf,
        train.args=list(
            method="ORFridge"
        )
    ),
    rrf=list(
        fs.fun=fs.rf,
        train.args=list(
            method="RRFglobal"
        )
    ),
    treebag=list(
        fs.fun=fs.rf,
        train.args=list(
            method="treebag"
        )
    ),
    c50=list(
        fs.fun=fs.tree,
        train.args=list(
            method="C5.0",
            tuneLength=6,
            control=C5.0Control(earlyStopping=F))
    )
)

# Common description
common.desc <- list(
    # Serialize models to models/ directory
    save.path="models",
    # Function to apply to the data frame
    fs.fun=fs.all,
    # Data
    data=dt.dmc,
    # Arguments to caret::train
    train.args=list(trControl=ctrl, metric="score", maximize=F)
)
