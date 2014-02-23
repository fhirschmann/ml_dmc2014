# Training function definition

source("R/pp.R")

suppressPackageStartupMessages(library(caret))

# Be deterministic
set.seed(42)

# Train control. Use 2-fold CV for testing.
ctrl <- trainControl(method="cv", number=2, classProbs=T, savePredictions=T)

# Use the control parameters defined above
mytrain <- function(...) {
    train(voucher ~ ., trControl=ctrl, ...)
}

# The list of training functions
trainers <- list()
trainers$nb <- function() mytrain(method="nb", data=dt2)
