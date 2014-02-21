source("R/common.R")
dmc.libs()
source("R/caret.R")

# Be deterministic
set.seed(42)

# Train control. We use 10-fold Cross-Validation and a custom summary
# function (see R/caret.R) that provides a variety of metrics.
ctrl <- trainControl(method="cv", savePredictions=TRUE) #, summaryFunction=mysummary)

# Use the control parameters defined above
mytrain <- function(form, method=method, ...) {
    train(form, method=method, trControl=ctrl, ...)
}

# Read in some data
dt <- read.csv("task2010/dmc2010_train.txt", sep=";")
# TODO: Read date and time columns as POSIX timestamps etc...

# The list of training functions
trainers <- list()

trainers$svmPoly <- function(form, ...) mytrain(form, method="svmPoly", tuneLength=8, ...)
trainers$lm <- function(form, ...) mytrain(form, method="lm", preProc=c("center", "scale"))

# To be continued
