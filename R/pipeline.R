source("R/common.R")
dmc.libs()
source("R/caret.R")

# Be deterministic
set.seed(42)

# Train control. Use 2-fold CV for testing.
ctrl <- trainControl(method="cv", number=2, classProbs=T, savePredictions=T,
                     summaryFunction=twoClassSummary)

# Use the control parameters defined above
mytrain <- function(form, method=method, ...) {
    train(form, method=method, trControl=ctrl, ...)
}

# Read in some data
dt <- read.csv("task2010/dmc2010_train.txt", sep=";")

## Nominal Columns (factors)
dt_factors <- c("customernumber", "salutation", "title",
                "domain", "model", "newsletter", "paymenttype", "deliverytype",
                "invoicepostcode", "delivpostcode", "voucher",
                "advertisingdatacode", "case", "numberitems",
                "gift", "entry", "points", "shippingcosts", "target90")
dt[dt_factors] <- lapply(dt[dt_factors], as.factor)

## Dates
dt_dates <- c("date", "datecreated", "deliverydatepromised", "deliverydatereal")
dt[dt_dates] <- lapply(dt[dt_dates], as.Date)

# Set empty strings to NA
dt[dt$delivpostcode == "", ]$delivpostcode <- NA
dt[dt$advertisingdatacode == "", ]$advertisingdatacode <- NA

# If somebody wants to try Weka
#write.arff(dt, "task2010/dmc2010_train.arff")

# Attributes to use
# It does not work when using all attributes yet, have to find out which of
# the attributes is causing the problems. We should probably do some analysis
# as to which attributes to use first anyway
dt2 <- dt[c("voucher", "salutation", "title", "domain", "model", "newsletter")]

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