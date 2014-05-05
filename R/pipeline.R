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
    fda=list( #"Argumente implizieren unterschiedliche Anzahl Zeilen: 37, 96"
        fs.fun=fs.all,
        method="fda",
        tuneLength=5
    ),
    avNNet=list( #"konnte funktion nnet nicht finden"
        fs.fun=fs.all,
        method="avNNet",
        tuneLength=5
    ),
    amore=list( #"Fehler in order(..) : Argument 1 ist kein Vektor
        fs.fun=fs.nn,
        method="AMORE",
        tuneLength=5
    ),
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
        tuneLength=4
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
    knn=list(
        fs.fun=fs.rf,
        method="knn"
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
    nb=list(
        fs.fun=fs.nb,
        method="nb"
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
