### Data Preprocessing goes here ###

# Read in some data
dt <- read.csv("task2010/dmc2010_train.txt", sep=";")
dt <- head(dt, 1000)

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

# Attributes to use
# It does not work when using all attributes yet, have to find out which of
# the attributes is causing the problems. We should probably do some analysis
# as to which attributes to use first anyway
dt2 <- dt[c("voucher", "salutation", "title", "domain", "model", "newsletter")]

# Add some features
dt2$deliverydatediff <- dt$deliverydatepromised - dt$deliverydatereal
dt2$allbuys <- dt$w0 + dt$w1 + dt$w2 + dt$w3 + dt$w4 + dt$w5 + dt$w6 + dt$w7 + dt$w8 + dt$w9 + dt$w10