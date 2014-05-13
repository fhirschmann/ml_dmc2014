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
    shrinkage=c(0.001),
    n.trees=c(400, 500, 600, 700, 800, 900, 1000),
    interaction.depth=5:10
)

grid.gbm2 <- expand.grid(
    shrinkage=c(0.05),
    n.trees=c(400, 500, 600, 700, 800),
    interaction.depth=7
)

# List of stuff to learn
descs <- list(
    ## C5.0 on M30
    c50M30=list(
        fs.fun=fs.c50,
        method="C5.0",
        tuneGrid=expand.grid(
            model="rules",
            winnow=F,
            trials=20
        )
    ),
    ## C5.0 on 3M31
    c50M31=list(
        fs.fun=fs.c50,
        method="C5.0",
        tuneGrid=expand.grid(
            model="rules",
            winnow=F,
            trials=40
        )
    ),
    c50M3150=list(
        fs.fun=fs.c50,
        method="C5.0",
        tuneGrid=expand.grid(
            model="rules",
            winnow=F,
            trials=50
        )
    ),
    ## MARS on M30
    earthM30=list(
        fs.fun=fs.stat,
        method="earth",
        tuneGrid=expand.grid(
            degree=2,
            nprune=12
        ),
        trControl=ctrl.probs
    ),
    ## MARS on M11
    earthM31=list(
        fs.fun=fs.stat,
        method="earth",
        tuneGrid=expand.grid(
            degree=2,
            nprune=18
        ),
        trControl=ctrl.probs
    ),	
    elmM30=list(
        trControl=ctrl.probs,
        fs.fun=fs.stat,
        tuneGrid=expand.grid(
            nhid=157,
            actfun="purelin"
        ),
        method="elm"
    ),
    elmM31=list(
        trControl=ctrl.probs,
        fs.fun=fs.stat,
        tuneGrid=expand.grid(
            nhid=167,
            actfun="purelin"
        ),
        method="elm"
    ),
	ensemble=list(
		fs.fun=fs.ensemble,
		method="C5.0",
        tuneGrid=expand.grid(
            model="rules",
            winnow=F,
            trials=20
        )
	),
	earthSimon=list(
		fs.fun=fs.simon,
        method="earth",
        tuneGrid=expand.grid(
            degree=2,
            nprune=12
        ),
        trControl=ctrl.probs
	),
	pdaSimon=list(
        fs.fun=fs.simon,
        method="pda",
        preProcess=c("center", "scale"),
        tuneGrid=expand.grid(lambda=1),
        trControl=ctrl.probs
    ),
    pda=list(
        fs.fun=fs.stat,
        method="pda",
        preProcess=c("center", "scale"),
        tuneGrid=expand.grid(lambda=1),
        trControl=ctrl.probs
    ),
    # GBM
    gbm=list(
        fs.fun=fs.gbm,
        method="gbm",
        tuneGrid=grid.gbm,
        trControl=ctrl.probs
    ),
	gbm2=list(
	    fs.fun=fs.gbm,
	    method="gbm",
	    tuneGrid=grid.gbm2,
	    trControl=ctrl.probs
	),
    gbmM31=list(
        fs.fun=fs.gbm,
        method="gbm",
        tuneGrid=expand.grid(
            shrinkage=0.01,
            interaction.depth=8,
            n.trees=550
        ),
        trControl=ctrl.probs
    ),
    gbmM30=list(
        fs.fun=fs.gbm,
        method="gbm",
        tuneGrid=expand.grid(
            shrinkage=0.05,
            interaction.depth=7,
            n.trees=500
        ),
        trControl=ctrl.probs
    ),
    avNNet=list(
        fs.fun=fs.stat,
        MaxNWts=20000,
        method="avNNet"
    ),
    stepLDA=list(
        fs.fun=fs.stat,
        method="stepLDA"
    ),
    mlp=list(
        fs.fun=fs.stat,
        tuneLength=2,
        method="mlp"
    ),
    elm=list(
        fs.fun=fs.stat,
        tuneLength=200,
        method="elm"
    ),
    pls=list(
        fs.fun=fs.stat,
        tuneLength=2,
        method="kernelpls"
    ),
    pam=list(
        fs.fun=fs.stat,
        tuneLength=2,
        method="pam"
    ),
    pda=list(
        fs.fun=fs.stat,
        tuneLength=2,
        method="pda"
    ),
    rbf=list(
        fs.fun=fs.stat,
        tuneLength=2,
        method="rbf"
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
