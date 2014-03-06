source("R/pp.R")
source("R/fs.R")

# Lazy evaluation: dt and dt.test get constructed when needed
delayedAssign("dt", cl(pp(read.csv("task2010/dmc2010_train.txt", sep=";"))))
delayedAssign("dt.test", cl(pp(read.csv("task2010/dmc2010_class.txt", sep=";"))))

delayedAssign("dt.c50", fs.c50(dt))
delayedAssign("dt.cart", fs.cart(dt))

# Merged Train/Test Data; this is mostly useful for plotting
delayedAssign("dt.merged", rbind(data.frame(dt, group="train"),
                                 data.frame(dt.test, group="test", target90=NA)))