# Training function definition
source("R/dmc.R")
source("R/fs.R")
source("R/feat.R")

library(functional)
library(caret)
library(C50)

ctrl <- trainControl(returnData=T, returnResamp="all")

# Don't pass this to classifiers who do not return class probabilities
ctrl.probs <- ctrl
ctrl.probs$classProbs <- TRUE

# List of stuff to learn
descs <- list(
    ada=list(
        fs.fun=fs.tree,
        method="ada"
    ),
    amore=list(
        fs.fun=fs.nn,
        method="AMORE"
    ),
    gbm=list(
        fs.fun=fs.tree,
        method="gbm",
        tuneLength=5
    ),
    svmLinear=list(
        fs.fun=fs.svm,
        method="svmLinear"
    ),
    nb=list(
        fs.fun=fs.nb,
        method="nb"
    ),
    rf=list(
        fs.fun=fs.rf,
        method="rf",
        tuneLength=2
    ),
    rf2=list(
        fs.fun=fs.rf,
        method="extraTrees",
        tuneLength=1
    ),
    orf=list(
        fs.fun=fs.rf,
        method="ORFridge"
    ),
    rrf=list(
        fs.fun=fs.rf,
        method="RRFglobal"
    ),
    treebag=list(
        fs.fun=fs.rf,
        method="treebag"
    ),
    svmLinear=list(
        fs.fun=fs.svm,
        method="svmLinear",
        tuneLength=5
    ),
    evtree=list(
        fs.fun=fs.tree,
        method="evtree"
    ),
    ctree=list(
        fs.fun=fs.tree,
        method="ctree"
    ),
    c50=list(
        fs.fun=fs.tree,
        method="C5.0",
        #tuneLength=5
        tuneGrid=expand.grid(
            trials=1, winnow=c(F), model="rules"),
        control=C5.0Control(earlyStopping=F)
    ),
    c50t=list(
        fs.fun=fs.tree,
        method="C5.0",
        tuneLength=10,
        control=C5.0Control(earlyStopping=F)
    )
)

# Common description
common.desc <- list(
    # Serialize models to models/ directory
    save.path="models",
    # Function to apply to the data frame
    fs.fun=fs.all,
    # Data
    trControl=ctrl
)
