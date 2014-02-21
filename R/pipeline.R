### This file defines the training functions ###

source("R/common.R")
dmc.libs()
source("R/caret.R")
source("R/pp.R")

# Be deterministic
set.seed(42)

# Train control. Use 2-fold CV for testing.
ctrl <- trainControl(method="cv", number=2, classProbs=T, savePredictions=T,
                     summaryFunction=twoClassSummary)

# Use the control parameters defined above
mytrain <- function(form, method=method, ...) {
    train(form, method=method, trControl=ctrl, ...)
}

# The list of training functions
trainers <- list()
trainers$nb <- function(form, ...) mytrain(form, method="nb", ...)

for (name in names(trainers)) {
    # Learn the attribute `voucher` using the domain and the salutation
    fit <- trainers[[name]](voucher ~ ., data=dt2)

    # Save model to a file.
    fname <- file.path("models", paste(name, ".RData", sep=""))
    save(fit, file=fname)

    print(fit)
}