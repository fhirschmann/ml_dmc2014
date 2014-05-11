# Training function definition
source("R/dmc.R")
source("R/fs.R")
source("R/feat.R")

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

grid.gbmS <- expand.grid(
    shrinkage=c(0.1, 0.01, 0.001),
    interaction.depth=5:6,
    n.trees=c(200,340, 400)
)

grid.gbmS2 <- expand.grid(
    shrinkage=c(0.001),
    interaction.depth=6,
    n.trees=c(200,340)
)

grid.gbmS3 <- expand.grid(
    shrinkage=c(0.001),
    interaction.depth=7,
    n.trees=c(300,400,500)
)

grid.gbmS31 <- expand.grid(
    shrinkage=c(0.001),
    interaction.depth=5:6,
    n.trees=c(500,600)
)

grid.gbmS4 <- expand.grid(
    shrinkage=c(0.001),
    interaction.depth=5,
    n.trees=c(300,400,500)
)

grid.gbmS5 <- expand.grid(
    shrinkage=c(0.01),
    interaction.depth=7,
    n.trees=c(200, 300,400,500)
)

grid.gbmS51 <- expand.grid(
    shrinkage=c(0.01),
    interaction.depth=5:6,
    n.trees=c(400,500)
)

grid.gbmS52 <- expand.grid(
    shrinkage=c(0.01),
    interaction.depth=7,
    n.trees=c(600,800)
)

grid.gbmS6 <- expand.grid(
    shrinkage=c(0.01),
    interaction.depth=2:5,
    n.trees=c(200, 300,400,500)
)

grid.c50 <- expand.grid(
    model=c("rules", "tree"),
    winnow=c(T, F),
    trials=c(1, 1:10 * 10)
)

grid.earthC <- expand.grid(
	degree=c(1,2),
	nprune=c(5,7,8,9,10,12,15)
)


# List of stuff to learn
descs <- list(
    # Final (tuned) Descriptions
    
    ## C5.0 on M10
    c50FM10=list(
        fs.fun=fs.c50,
        method="C5.0",
        tuneGrid=expand.grid(
            model="rules",
            winnow=F,
            trials=30
        )
    ),
    
    # Stuff to run on the Cluster: Postfix with 'C'
    
    # GBM
    gbmC=list(
        fs.fun=fs.gbm,
        method="gbm",
        tuneGrid=grid.gbm
    ),
    
    gbmCs=list(
        fs.fun=fs.gbm,
        method="gbm",
        tuneGrid=grid.gbmS
    ),
	
	gbmCs2=list(
        fs.fun=fs.gbm,
        method="gbm",
        tuneGrid=grid.gbmS2
    ),
	
	gbmCs3=list(
        fs.fun=fs.gbm,
        method="gbm",
        tuneGrid=grid.gbmS3,
		trControl=ctrl.probs
    ),
	
	gbmCs31=list(
        fs.fun=fs.gbm,
        method="gbm",
        tuneGrid=grid.gbmS31,
		trControl=ctrl.probs
    ),
	
	gbmCs4=list(
        fs.fun=fs.gbm,
        method="gbm",
        tuneGrid=grid.gbmS4,
		trControl=ctrl.probs
    ),
	
	gbmCs5=list(
        fs.fun=fs.gbm,
        method="gbm",
        tuneGrid=grid.gbmS5,
		trControl=ctrl.probs
    ),
	
	gbmCs51=list(
        fs.fun=fs.gbm,
        method="gbm",
        tuneGrid=grid.gbmS51,
		trControl=ctrl.probs
    ),
	
	gbmCs52=list(
        fs.fun=fs.gbm,
        method="gbm",
        tuneGrid=grid.gbmS52,
		trControl=ctrl.probs
    ),
	
	gbmCs6=list(
        fs.fun=fs.gbm,
        method="gbm",
        tuneGrid=grid.gbmS6,
		trControl=ctrl.probs
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
        control=C5.0Control(earlyStopping=F)
    ),
    
    earthC=list(
        fs.fun=fs.stat,
        method="earth",
        tuneGrid=grid.earthC,
        trControl=ctrl.probs
    ),
    
    bagEarthC=list(
        fs.fun=fs.stat,
        tuneGrid=grid.earthC,
        method="bagEarth"
    ),
    
    gamboost=list(
        fs.fun=fs.stat,
        tuneLength=6,
        trControl=ctrl.probs,
        method="gamboost"
        
    ),
    
    ctree2=list(
        fs.fun=fs.tree,
        tuneLength=3,
        method="ctree"
    ),
    
    rrlda=list(
        fs.fun=fs.stat,
        tuneLength=2,
        trControl=ctrl.probs,
        method="rrlda"
    ),
    
    RSimca=list(
        fs.fun=fs.stat,
        tuneLength=2,
        method="RSimca"
    ),
    
    nh=list(
        fs.fun=fs.tree,
        tuneLength=3,
        method="nodeHarvest"
    ),
    
    ORFlog=list(
        fs.fun=fs.rf,
        tuneLength=2,
        method="ORFlog"
    ),
    
    cforest=list(
        fs.fun=fs.rf,
        method="cforest",
        tuneLength=4
    ),
    
    bagFDAC=list(
        fs.fun=fs.stat,
        tuneLength=6,
        method="earth"
    ),
    
    avNNet=list(
        fs.fun=fs.nn,
        MaxNWts=20000,
        method="avNNet"
    ),
    
    mlpC=list(
        fs.fun=fs.stat,
        method="mlp",
        tuneLength=4
    ),
        
    dsaC=list(
        fs.fun=fs.stat,
        method="partDSA",
        tuneLength=3
    ),
    
    pdaC=list(
        fs.fun=fs.stat,
        method="pda",
        tuneLength=5,
        trControl=ctrl.probs
    ),
    
    rdaC=list(
        fs.fun=fs.stat,
        method="rocc",
        tuneLength=1
    ),
    
    # C5.0 with Probabilities
    c50CP=list(
        fs.fun=fs.tree,
        method="C5.0",
        tuneGrid=grid.c50,
        trControl=ctrl.probs,
        control=C5.0Control(earlyStopping=T)
    ),
    
    C50T1=list(
        fs.fun=fs.c50,
        method="C5.0",
        tuneGrid=expand.grid(trials=1:5, winnow=F, model="rules"),
        control=C5.0Control(CF=0.1)
    ),
    C50T2=list(
        fs.fun=fs.c50,
        method="C5.0",
        tuneGrid=expand.grid(trials=1:5, winnow=F, model="rules"),
        control=C5.0Control(CF=0.2)
    ),
    C50T3=list(
        fs.fun=fs.c50,
        method="C5.0",
        tuneGrid=expand.grid(trials=1:5, winnow=F, model="rules"),
        control=C5.0Control(CF=0.3)
    ),
    C50T4=list(
        fs.fun=fs.c50,
        method="C5.0",
        tuneGrid=expand.grid(trials=1:5, winnow=F, model="rules"),
        control=C5.0Control(CF=0.4)
    ),
    C50T5=list(
        fs.fun=fs.c50,
        method="C5.0",
        tuneGrid=expand.grid(trials=1:5, winnow=F, model="rules"),
        control=C5.0Control(CF=0.5)
    ),
    C50T6=list(
        fs.fun=fs.c50,
        method="C5.0",
        tuneGrid=expand.grid(trials=1:5, winnow=F, model="rules"),
        control=C5.0Control(CF=0.6)
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
        trControl=ctrl.probs,
        tuneLength=6
    ),
    
    # Support Vector Machine (Polynomial Kernel)
    svmPolyC=list(
        fs.fun=fs.rf,
        method="svmPoly",
        tuneLength=2#7
    ),
    
    # Bagged Cart
    treebagC=list(
        fs.fun=fs.tree,
        method="treebag",
        tuneLength=2
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
        tuneLength=3
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
