source("R/pp.R")
source("R/utils.R")

na.strings <- c("NA", "", "??", "?")

# Lazy evaluation: dt and dt.test get constructed when needed
delayedAssign("dt.raw", read.csv("task/orders_train.txt",
                                 sep=";", na.strings=na.strings))
delayedAssign("dt.unclean", pp(dt.raw))
delayedAssign("dt", addlevels(im(cl(dt.unclean)), dt.test.unclean))


delayedAssign("dt.test.raw", read.csv("task/orders_class.txt",
                                      sep=";", na.strings=na.strings))
delayedAssign("dt.test.unclean", pp(dt.test.raw))
delayedAssign("dt.test", addlevels(im(cl(dt.test.unclean)), dt.unclean))

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