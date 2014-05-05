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
    hda=list(
        fs.fun=fs.rf,
        method="hda"
    ),
    fda=list(
        fs.fun=fs.nn,
        method="fda"
    ),
    gaussL=list(
        fs.fun=fs.all,
        method="gaussprLinear",
        tuneLength=5
    ),
    multinom=list(
        fs.fun=fs.rf,
        method="multinom"
    ),
    gbm=list(
        fs.fun=fs.gbm,
        method="gbm",
        tuneLength=4,
        n.minobsinnode=3
    ),
    gbmS=list(
        fs.fun=fs.gbm,
        method="gbm",
        tuneGrid=expand.grid(
            shrinkage=c(0.0001, 0.001, 0.01),
            interaction.depth=1:7,
            n.trees=1:10 * 30
        )
    ),
    gbmT=list(
        fs.fun=fs.gbm,
        method="gbm",
        tuneGrid=expand.grid(
            shrinkage=c(0.00001, 0.0001, 0.001, 0.01, 0.1),
            interaction.depth=1:10,
            n.trees=1:10 * 50
        )
    ),
    mlp=list(
        fs.fun=fs.rf,
        method="mlp"
    ),
    mda=list(
        fs.fun=fs.rf,
        method="mda"
    ),
    lmt=list(
        fs.fun=fs.tree,
        method="LMT"
    ),
    lboost=list(
        fs.fun=fs.rf,
        method="LogitBoost"
    ),
    j48=list(
        fs.fun=fs.tree,
        method="J48"
    ),
    jrip=list(
        fs.fun=fs.tree,
        method="JRip"
    ),
    svmLinear=list(
        fs.fun=fs.svm,
        method="svmLinear"
    ),
    rf=list(
        fs.fun=fs.rf,
        method="rf",
        tuneLength=2
    ),
    rfC=list(
      fs.fun=fs.rf,
      method="rf",
      tuneLength=15
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
        fs.fun=fs.rf,
        method="svmLinear",
        tuneLength=5
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
        tuneLength=4,
        control=C5.0Control(earlyStopping=F)
    ),
    c50C=list(
      fs.fun=fs.tree,
      method="C5.0",
      tuneLength=15,
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
