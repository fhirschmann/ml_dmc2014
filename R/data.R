source("R/pp.R")
source("R/utils.R")

na.strings <- c("NA", "", "??", "?")

# Lazy evaluation: dt and dt.test get constructed when needed
delayedAssign("dt.train.raw", read.csv("task/orders_train.txt",
                                       sep=";", na.strings=na.strings))
delayedAssign("dt.train.pp", pp(dt.raw))
delayedAssign("dt.train", addlevels(im(cl(dt.train.pp)), dt.test.pp))


delayedAssign("dt.test.raw", read.csv("task/orders_class.txt",
                                      sep=";", na.strings=na.strings))
delayedAssign("dt.test.pp", pp(dt.test.raw))
delayedAssign("dt.test", addlevels(im(cl(dt.test.pp)), dt.train.pp))

# Merged Train/Test Data; this is mostly useful for plotting
delayedAssign("dt.merged", rbind(data.frame(dt, group="train"),
                                 data.frame(dt.test, group="test", returnShipment=NA)))

# Serialized Models
delayedAssign("mds", caret.load())


clear <- function() {
    rm(list=c(dt.raw, dt, dt.unclean,
              dt.test.raw, dt.test, dt.test.unclean,
              dt.merged, mds))
}