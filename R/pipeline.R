# Training function definition

source("R/pp.R")
source("R/dmc2010.R")

suppressPackageStartupMessages(library(caret))

# Be deterministic
set.seed(42)

# Train control. Use 2-fold CV for testing.
ctrl <- trainControl(method="cv", number=10, classProbs=T, savePredictions=T,
                     summaryFunction=dmc.summary)

# Use the control parameters defined above
mytrain <- function(...) {
    train(target90 ~ ., maximize=T, metric="Points", trControl=ctrl, ...)
}

weights <- ifelse(dt$voucher == "yes", -1, 5)

# The list of training functions
trainers <- list()
#trainers$nb <- function() mytrain(method="nb", data=dt2, weights=weights)
trainers$c50 <- function() mytrain(method="C5.0Cost", data=dt2, cost=dmc.cost)
trainers$cart <- function() mytrain(method="rpart", data=dt2,
                                    # rpart wants the matrix to have true classes
                                    # in rows, so we transpose
                                    parms=list(loss=t(dmc.cost)))


run <- function(name) {
    fit <- trainers[[name]]()
    
    fname <- file.path("models", paste(name, ".RData", sep=""))
    save(fit, file=fname)
    
    cat("Wrote model for", name, "to", fname, "\n")
    fit
}