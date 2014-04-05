source("R/pp.R")
source("R/utils.R")

na.strings <- c("NA", "", "??", "?")

# Lazy evaluation: dt.train and dt.test get constructed when needed
delayedAssign("dt.train.raw", read.csv("task/orders_train.txt",
                                       sep=";", na.strings=na.strings))
delayedAssign("dt.train.pp", pp(dt.train.raw))
delayedAssign("dt.train", addlevels(im(cl(dt.train.pp)), dt.test.pp))

delayedAssign("dt.june", subset(dt.train, month(orderDate) == 6))
delayedAssign("dt.mini", subset(dt.train, week(orderDate) == 6))
delayedAssign("dt.tiny", subset(dt.mini, wday(orderDate) == 1))

delayedAssign("dt.test.raw", read.csv("task/orders_class.txt",
                                      sep=";", na.strings=na.strings))
delayedAssign("dt.test.pp", pp(dt.test.raw))
delayedAssign("dt.test", addlevels(im(cl(dt.test.pp)), dt.train.pp))

# Merged Train/Test Data; this is mostly useful for plotting
delayedAssign("dt.merged", rbind(data.frame(dt.train, group="train"),
                                 data.frame(dt.test, group="test", returnShipment=NA)))

# Serialized Models
delayedAssign("mds", caret.load())


clear <- function() {
    rm(list=c(dt.train.raw, dt.train.pp, dt.train, dt.june,
              dt.test.raw, dt.test.pp, dt.test,
              dt.merged, mds))
}