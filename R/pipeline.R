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
    interaction.depth=4:5,
    n.trees=c(200, 300,400,500)
)

grid.svmRadial <- expand.grid(
	C=c(2^-5,2^-4,2^-3,2^-2,2^-1,1,2^1,2^2,2^3,2^4,2^5,2^6,2^7),
	sigma=c(2^2,2^1,1,2^-1,2^-2,2^-3,2^-4,2^-5,2^-6,2^-7,2^-8,2^-9)
)
	
grid.svmRadialCost <- expand.grid(
	C=c(2^-5,2^-4,2^-3,2^-2,2^-1,1,2^1,2^2,2^3,2^4,2^5,2^6,2^7,0.1,0.01,0.001,0.0001,0.0005,0.005)
	)

# List of stuff to learn
descs <- list(
    # Final (tuned) Descriptions
	
	## random forest with integrated feature selection
	boruta=list(
		fs.fun=fs.rf,
		method="Boruta",
		tuneLenght=3
	),
	
	## svms: least squares versions
	lssvmLinear=list( ## doesnt work atm
		fs.fun=fs.stat,
		method="lssvmLinear",
		preProcess=c("center","scale"),
		tuneLength=33
	),
	
	svmLinear=list(
        fs.fun=fs.stat,
        method="svmLinear",
		preProcess=c("center","scale"),
        tuneGrid=grid.svmRadialCost
    ),
	
	svmRadial=list( 
		fs.fun=fs.stat,
		method="svmRadial",
		preProcess=c("center","scale"),
		tuneGrid=grid.svmRadial
	),
	
	svmRadialCost=list(
		fs.fun=fs.stat,
		method="svmRadialCost",
		preProcess=c("center","scale"),
		tuneGrid=grid.svmRadialCost
	),
    
    pda=list(
        fs.fun=fs.stat,
        method="pda",
        preProcess=c("center", "scale"),
        tuneGrid=expand.grid(lambda=1),
        trControl=ctrl.probs
    ),
    
    ## C5.0 on M30
    c50M30=list(
        fs.fun=fs.c50,
        method="C5.0",
        tuneGrid=expand.grid(
            model="rules",
            winnow=F,
            trials=c(20, 25, 28, 30, 32, 35, 40)
        )
    ),
    
    ## C5.0 on 3M31
    c50M31=list(
        fs.fun=fs.c50,
        method="C5.0",
        tuneGrid=expand.grid(
            model="rules",
            winnow=F,
            trials=c(20, 25, 38, 39, 40, 45, 50)
        )
    ),
    
    ## MARS on M30
    earthM30=list(
        fs.fun=fs.stat,
        method="earth",
        tuneGrid=expand.grid(
            degree=2,
            nprune=7:14
        ),
        trControl=ctrl.probs
    ),
    
    ## MARS on M11
    earthM31=list(
        fs.fun=fs.stat,
        method="earth",
        tuneGrid=expand.grid(
            degree=2,
            nprune=14:20
        ),
        trControl=ctrl.probs
    ),
    
    ## Bagged MARS on M10
    bearthM30=list(
        fs.fun=fs.stat,
        method="bagEarth",
        tuneGrid=expand.grid(
            degree=2,
            nprune=7:14
        ),
        trControl=ctrl.probs
    ),
    bearthM31=list(
        fs.fun=fs.stat,
        method="bagEarth",
        tuneGrid=expand.grid(
            degree=2,
            nprune=7:14
        )
    ),
    bearthM30=list(
        fs.fun=fs.stat,
        method="bagEarth",
        tuneGrid=expand.grid(
            degree=2,
            nprune=7:14
        )
    ),
    bearthM30=list(
        fs.fun=fs.stat,
        method="bagEarth",
        tuneGrid=expand.grid(
            degree=2,
            nprune=14:20
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
    
    gbmCMx0=list(
        fs.fun=fs.gbm,
        method="gbm",
        tuneGrid=expand.grid(
            shrinkage=c(0.01),
            interaction.depth=7,
            n.trees=500
            )
    ),
    gbmCMx1=list(
        fs.fun=fs.gbm,
        method="gbm",
        tuneGrid=expand.grid(
            shrinkage=c(0.01),
            interaction.depth=8,
            n.trees=c(500, 550, 600, 650)
            )
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
    gbmCPMx0=list(
        fs.fun=fs.gbm,
        method="gbm",
        tuneGrid=expand.grid(
            shrinkage=c(0.01),
            interaction.depth=7,
            n.trees=500
            ),
        trControl=ctrl.probs
    ),
    gbmCPMx1=list(
        fs.fun=fs.gbm,
        method="gbm",
        tuneGrid=expand.grid(
            shrinkage=c(0.01),
            interaction.depth=8,
            n.trees=c(500, 550, 600, 650)
            ),
        trControl=ctrl.probs
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
        tuneLength=6,
        trControl=ctrl.probs,
        method="rrlda"
    ),
    
    RSimca=list(
        fs.fun=fs.stat,
        tuneLength=6,
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
    
    rdaC=list(
        fs.fun=fs.stat,
        method="rocc",
        tuneLength=1
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
        tuneLength=4
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
    rf=list(
        fs.fun=fs.rf,
        method="rf",
        tuneLength=5
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
        method="treebag",
        tuneLength=5
    ),
    
    ctree=list(
        fs.fun=fs.tree,
        method="ctree",
        tuneLength=5
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
