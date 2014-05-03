library(lubridate)

source("R/utils.R")

#load("data.RData")

# Lazy evaluation: dt.train and dt.test get constructed when needed

delayedAssign("dt.train", readRDS("data.train.RData"))
delayedAssign("dt.test", readRDS("data.test.RData"))
delayedAssign("dt.dmc", readRDS("data.dmc.RData"))
delayedAssign("dt.dmc.ids", readRDS("data.dmc.ids.RData"))

delayedAssign("dt.train.ae", subset(dt.train, orderDate >= as.Date("2012-04-09") & 
                                        orderDate < (as.Date("2012-04-09") + days(30))))

delayedAssign("dt.june", subset(dt.train, month(orderDate) == 6))
delayedAssign("dt.july", subset(dt.train, month(orderDate) == 7))
delayedAssign("dt.august", subset(dt.train, month(orderDate) == 8))

delayedAssign("dt.mini", subset(dt.train, week(orderDate) == 6))
delayedAssign("dt.tiny", subset(dt.mini, wday(orderDate) == 1))

# Merged Train/Test Data; this is mostly useful for plotting
delayedAssign("dt.merged", rbind(data.frame(dt.train, group="train"),
                                 data.frame(dt.test, group="test", returnShipment=NA)))

delayedAssign("dt.dmc.small", sapply(dt.dmc, function(x) list(train=head(x$train, 20000), test=head(x$test, 2000)),
                                     simplify=F))
delayedAssign("dt.dmc.mini", sapply(dt.dmc, function(x) list(train=head(x$train, 2000), test=head(x$test, 100)),
                                    simplify=F))