# Training function definition

source("R/pp.R")
source("R/dmc2010.R")

suppressPackageStartupMessages(library(caret))

# Be deterministic
set.seed(42)

# Train control. Use 2-fold CV for testing.
ctrl <- trainControl(method="cv", number=2, classProbs=T, savePredictions=T,
                     summaryFunction=dmc.summary)

# Use the control parameters defined above
mytrain <- function(...) {
    train(voucher ~ ., maximize=T, trControl=ctrl, ...)
}

weights <- ifelse(dt$voucher == "yes", -1, 5)

# The list of training functions
trainers <- list()
trainers$nb <- function() mytrain(method="nb", data=dt2, weights=weights)
trainers$c50 <- function() mytrain(method="C5.0", data=dt2, weights=weights)
