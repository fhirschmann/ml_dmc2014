# Training function definition
source("R/dmc.R")
source("R/fs.R")
source("R/feat.R")

library(functional)
library(caret)
library(C50)

ctrl <- trainControl(method="cv", savePredictions=T,
                     summaryFunction=dmc.summary, returnData=T,
                     index=dmc.dtmap(dt.dmc.ids$train, "Training"),
                     indexOut=dmc.dtmap(dt.dmc.ids$test, "Testing"))

# Don't pass this to classifiers who do not return class probabilities
ctrl.probs <- ctrl
ctrl.probs$classProbs <- TRUE

# This will use the otf features
dt.T <- rbind(dt.dmc$T1$train, dt.dmc$T1$test,
              dt.dmc$T2$train, dt.dmc$T2$test,
              dt.dmc$T3$train, dt.dmc$T3$test)
rownames(dt.T) <- dt.T$orderItemID

# List of stuff to learn
descs <- list(
    nb=list(
        train.args=list(
            method="nb",
            trControl=ctrl.probs)
    ),
    c50=list(
        data.fun=fs.tree,
        na.action=na.pass,
        train.args=list(
            method="C5.0",
            tuneGrid=expand.grid(
                .winnow=c(FALSE),
                .model=c("rules"),
                .trials=1
                ),
            trControl=ctrl,
            na.action=na.pass,
            control=C5.0Control(earlyStopping=F)))
)

# Common description
common.desc <- list(
    # Serialize models to models/ directory
    serialize="models",
    # Keep a history of trained serialized models
    save.hist=T,
    # Keep a history of the data frame that was used for training
    save.data=T,
    # Function to apply to the data frame
    data.fun=fs.all,
    # Common arguments to caret::train
    train.args=list(
        # Always learn returnShipment using all attributes
        form=returnShipment ~ .,
        # Data to Train on
        data=dt.T,
        # Maximize the metric
        maximize=F,
        # Use Points to select the best tuning parameters
        metric="Score",
        # Save the predictions (can be used to calculate metrics later)
        trControl=ctrl)
)
