source("R/pp.R")
source("R/fs.R")

# Lazy evaluation: dt and dt.test get constructed when needed
delayedAssign("dt", pp(read.csv("task2010/dmc2010_train.txt", sep=";")))
delayedAssign("dt.test", pp(read.csv("task2010/dmc2010_class.txt", sep=";")))

delayedAssign("dt.c50", fs.c50(dt))
delayedAssign("dt.cart", fs.cart(dt))