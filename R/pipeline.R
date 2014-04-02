# Training function definition
source("R/dmc.R")
source("R/fs.R")

library(functional)
library(caret)
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
    c50=list(
        train.args=list(
            method="C5.0",
            tuneGrid=expand.grid(
                .winnow=c(FALSE),
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
        form=returnShipment ~ .,
        # Data to Train on
        data="dt",  # pass as string for lazy evaluation
        # Maximize the metric
        maximize=F,
        # Use Points to select the best tuning parameters
        metric="Score",
        # Save the predictions (can be used to calculate metrics later)
        trControl=ctrl)
)
