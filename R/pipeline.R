# Training function definition

source("R/pp.R")
source("R/fs.R")
source("R/dmc2010.R")

suppressPackageStartupMessages(library(caret))

# Work on 10% of the original data
set.seed(42)
dt2 <- dt[createDataPartition(dt$target90, p=0.1, list=FALSE),]

# Create folds explicitly
set.seed(42)
indx <- createFolds(dt2, k=10, returnTrain=TRUE)

ctrl <- trainControl(classProbs=F, savePredictions=T,
                     summaryFunction=dmc.summary, index=indx)

# Use the control parameters defined above
mytrain <- function(...) {
    train(target90 ~ ., maximize=T, metric="Points", trControl=ctrl, ...)
}

# The list of training functions
trainers <- list()
#trainers$nb <- function() mytrain(method="nb", data=dt2, weights=weights)
trainers$c50 <- function() mytrain(method="C5.0Cost", data=fs.c50(dt2), cost=dmc.cost,
                                   control=C5.0Control(earlyStopping=FALSE))
trainers$cart <- function() mytrain(method="rpart", data=fs.cart(dt2),
                                    # rpart wants the matrix to have true classes
                                    # in rows, so we transpose
                                    parms=list(loss=t(dmc.cost)))
