# Training function definition

source("R/pp.R")
source("R/fs.R")
source("R/dmc2010.R")

suppressPackageStartupMessages(library(caret))

# Create folds explicitly
set.seed(42)
ctrl <- trainControl(method="cv", classProbs=F, savePredictions=T,
                     summaryFunction=dmc.summary)

# Use the control parameters defined above
mytrain <- function(...) {
    train(target90 ~ ., maximize=T, metric="Points", trControl=ctrl, ...)
}

# The list of training functions
trainers <- list()
#trainers$nb <- function() mytrain(method="nb", data=dt2, weights=weights)
trainers$c50 <- function() mytrain(method="C5.0Cost", data=fs.c50(dt), cost=dmc.cost,
                                   control=C5.0Control(earlyStopping=FALSE))
trainers$cart <- function() mytrain(method="rpart", data=fs.cart(dt),
                                    # rpart wants the matrix to have true classes
                                    # in rows, so we transpose
                                    parms=list(loss=t(dmc.cost)))
