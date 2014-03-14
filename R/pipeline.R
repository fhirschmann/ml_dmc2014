# Training function definition
source("R/data.R")
source("R/dmc.R")

suppressPackageStartupMessages(library(caret))
library(C50)

# List of stuff to learn
descs <- list(
    nb=list(
        train.args=list(
            method="nb",
            data=dt.c50)
    ),
    c50=list(
        train.args=list(
            method="C5.0",
            data=dt.c50,
            cost=dmc.cost,
            tuneGrid=expand.grid(
                .winnow=c(FALSE),
                .model=c("rules"),
                .trials=c(1)
                ),
            control=C5.0Control(earlyStopping=F)))
)

# Common description
common.desc <- list(
    # Serialize models to models/ directory
    serialize="models",
    # Keep a history of trained serialized models
    hist=T,
    # For testing on a smaller data set
    data.fun=identity,
    #data.fun=function(x) head(x, 1000),
    # Common arguments to caret::train
    train.args=list(
        # Always learn target90 using all attributes
        form=target90 ~ .,
        # Maximize the metric
        maximize=T,
        # Use Points to select the best tuning parameters
        metric="PointsRatio",
        # Save the predictions (can be used to calculate metrics later)
        trControl=trainControl(method="cv", savePredictions=T,
                               summaryFunction=dmc.summary, returnData=T))
)