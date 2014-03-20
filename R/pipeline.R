# Training function definition
source("R/dmc.R")
source("R/fs.R")

library(functional)
library(C50)

ctrl <- trainControl(method="cv", savePredictions=T,
                     summaryFunction=dmc.summary, returnData=T)

# Don't pass this to classifiers who do not return class probabilities
ctrl.probs <- ctrl
ctrl.probs$classProbs <- TRUE

# List of stuff to learn
descs <- list(
    nb=list(
        train.args=list(
            method="nb",
            trControl=ctrl.probs)
    ),
    ada=list(
        train.args=list(
            method="ada")
    ),
    rf=list(
        train.args=list(
            method="rf",
            trControl=ctrl.probs)
    ),
    olf=list(
        train.args=list(
            method="ORFpls")
    ),
    evtree=list(
        train.args=list(
            method="evtree")
    ),
    gcvEarth=list(
        train.args=list(
            method="gcvEarth")
    ),
    lda=list(
        train.args=list(
            method="lda")
    ),
    svmPoly=list(
        train.args=list(
            method="svmPoly")
    ),
    svmLinear=list(
        train.args=list(
            method="svmLinear")
    ),
    svmRadial=list(
        train.args=list(
            method="svmRadial")
    ),
    OneR=list(
        train.args=list(
            method="OneR")
    ),
    bdk=list(
        train.args=list(
            method="bdk")
    ),
    nnet=list(
        train.args=list(
            method="nnet")
    ),
    c50=list(
        train.args=list(
            method="C5.0",
            cost=dmc.cost,
            tuneGrid=expand.grid(
                .winnow=c(FALSE, TRUE),
                .model=c("rules", "tree"),
                .trials=c(1:30)
                ),
            control=C5.0Control(earlyStopping=F)))
)

# Common description
common.desc <- list(
    # Serialize models to models/ directory
    serialize="models",
    # Keep a history of trained serialized models
    hist=T,
    # Function to apply to the data frame
    data.fun=fs.all,
    # For testing on a smaller data set
    #data.fun=Compose(fs.all, function(x) head(x, 1000)),
    # Common arguments to caret::train
    train.args=list(
        # Always learn target90 using all attributes
        form=target90 ~ .,
        # Data to Train on
        data="dt",  # pass as string for lazy evaluation
        # Maximize the metric
        maximize=T,
        # Use Points to select the best tuning parameters
        metric="PointsRatio",
        # Save the predictions (can be used to calculate metrics later)
        trControl=ctrl)
)
