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

## Nominal Columns (factors)
dt.factors <- c("customernumber", "salutation", "title",
                "domain", "model", "newsletter", "paymenttype", "deliverytype",
                "invoicepostcode", "delivpostcode", "voucher",
                "advertisingdatacode", "case", "numberitems",
                "gift", "entry", "points", "shippingcosts", "target90")
dt[dt.factors] <- lapply(dt[dt.factors], as.factor)

## Dates
dt.dates <- c("date", "datecreated", "deliverydatepromised", "deliverydatereal")
dt[dt.dates] <- lapply(dt[dt.dates], as.Date)

# Use a subset for testing
dt <- head(dt, 500)

# If somebody wants to try Weka
#write.arff(dt, "task2010/dmc2010_train.arff")

# The list of training functions
trainers <- list()
trainers$nb <- function(form, ...) mytrain(form, method="nb", ...)

for (name in names(trainers)) {
    # Learn the attribute `voucher` using the domain and the salutation
    fit <- trainers[[name]](voucher ~ ., data=dt[c("voucher", "domain", "salutation")])

    # Save model to a file.
    fname <- file.path("models", paste(name, ".RData", sep=""))
    save(fit, file=fname)

    print(fit)
}