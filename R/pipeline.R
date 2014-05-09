# Training function definition
source("R/dmc.R")
source("R/fs.R")
source("R/feat.R")

library(functional)
library(caret)
library(C50)

ctrl <- trainControl(returnData=F, returnResamp="all")

# Don't pass this to classifiers who do not return class probabilities
ctrl.probs <- ctrl
ctrl.probs$classProbs <- TRUE

grid.gbm <- expand.grid(
    shrinkage=c(0.1),
    interaction.depth=3:5,
    n.trees=c(180, 200, 220, 240, 260, 280, 300, 320, 340)
)

grid.c50 <- expand.grid(
    model=c("rules", "tree"),
    winnow=F,
    trials=1:20 * 3
)

# List of stuff to learn
descs <- list(
    # Stuff to run on the Cluster: Postfix with 'C'
    
    # GBM
    gbmC=list(keep.data=F,
        fs.fun=fs.gbm,
        method="gbm",
        tuneGrid=grid.gbm
    ),
    
    gbmCsimon=list(
        fs.fun=fs.simon,
        method="gbm",
        tuneGrid=grid.gbm
    ),
    
    nb=list(
      fs.fun=fs.nb,
      method="nb",
      tuneLength=3),
    
    # GBM with Probabilities
    gbmCP=list(
        fs.fun=fs.gbm,
        method="gbm",
        tuneGrid=grid.gbm,
        trControl=ctrl.probs
    ),
    
    # C5.0
    c50C=list(
        fs.fun=fs.c50,
        method="C5.0",
        tuneGrid=grid.c50,
        control=C5.0Control(earlyStopping=T)
    ),
    
    # C5.0 with Probabilities
    c50CP=list(
        fs.fun=fs.tree,
        method="C5.0",
        tuneGrid=grid.c50,
        trControl=ctrl.probs,
        control=C5.0Control(earlyStopping=T)
    ),
    
    # Penalized Multinomial Regression
    mnC=list(
        fs.fun=fs.rf,
        method="multinom",
        tuneLength=20
    ),
    
    # Regularized Random Forest (Global)
    rrfGC=list(
        fs.fun=fs.rf,
        method="RRFglobal",
        tuneLength=3#8
    ),
    
    # Support Vector Machine (Polynomial Kernel)
    svmPolyC=list(
        fs.fun=fs.rf,
        method="svmPoly",
        tuneLength=3#7
    ),
    
    # Bagged Cart
    treebagC=list(
        fs.fun=fs.tree,
        method="treebag",
        tuneLength=3
    ),
    fda=list(
        fs.fun=fs.nn,
        method="fda"
    ),
    gbm=list(
        fs.fun=fs.gbm,
        method="gbm",
        tuneLength=4,
        tuneGrid=expand.grid(
            shrinkage=0.001,
            interaction.depth=5,
            n.trees=100
        ),
        n.minobsinnode=3
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
        method="RRFglobal",
        tuneLength=3
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
    cforest=list(
        fs.fun=fs.rf,
        method="cforest",
        tuneLength=7
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
