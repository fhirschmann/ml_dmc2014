#!/usr/bin/env Rscript
library(vcd)
library(plyr)
source("R/data.R")

dt.merged <- rbind.fill(data.frame(dt.train, group = "train"), data.frame(dt.test, 
                                                                     group = "test", returnShipment = NA))

png("doc/pres_mosaic.png", width=5, height=6, units="in", res=300, bg="transparent")
mosaic(returnShipment ~ deliveryDateMissing, data=dt.train, shade=T, zero_size=1.0,
       main="Mosaic Plot")
dev.off()

png("doc/pres_sets.png", width=9, height=5, units="in", res=300, bg="transparent")

par(xpd=F)
someColors <- colorRampPalette(c("white", "orange", "red", "black"), space="Lab")
smoothScatter(as.integer(dt.merged$orderDate), as.integer(dt.merged$itemID), 
              main = "orderDate vs itemID", xlab = "Month", ylab = "itemID", xaxt = "n",
              colramp=someColors)

col.test <- "black"
start.test <- max(as.integer(dt.train$orderDate))
end.test <- max(as.integer(dt.test$orderDate))

start.dev <- min(as.integer(dt.dmc$M3$test$orderDate))
end.dev <- max(as.integer(dt.dmc$M3$test$orderDate))
col.dev <- "darkviolet"

abline(v=start.test, col=col.test)
abline(v=end.test, col=col.test)
abline(v=start.dev, col=col.dev)
abline(v=end.dev, col=col.dev)
arrows(start.dev, 500, end.dev, 500, col=col.dev)
arrows(start.test, 500, end.test, 500, col=col.test)
par(xpd = T)
#legend("topleft", c("Real Test Set", "Internal Test Set"), lty = c(1), col=c(col.test, col.dev))
legend("topleft", c("Real Test Set", "Internal Test Set"), lty = c(1, 1), col=c(col.test, col.dev))
lbls <- c(4:12, 1:5)
lbls2 <- as.Date(c("2012-04-01", "2012-05-01", "2012-06-01", "2012-07-01", "2012-08-01", 
                  "2012-09-01", "2012-10-01", "2012-11-01", "2012-12-01", "2013-01-01", "2013-02-01", 
                  "2013-03-01", "2013-04-01", "2013-04-30"))
axis(1, lbls2, lbls)

dev.off()
png("doc/pres_c50.png", width=9, height=7, units="in", res=300, bg="transparent")


c501 <- read.csv("doc/c50_M31.csv")
c500 <- read.csv("doc/c50_M30.csv")

c501 <- c501[c501$model == "tree" & c501$winnow == F, ]
c500 <- c500[c500$model == "tree" & c500$winnow == F, ]

plot(c500$trials, c500$accuracy, type="l", main="Parameter Tuning (C5.0)",
     xlab="Boosting Iterations", ylab="Accuracy", col="blue", ylim=c(min(c500$accuracy), max(c501$accuracy)))
lines(c501$trials, c501$accuracy, col="red")
legend(75, 0.675, c("New Customer", "Known Customer"), lty=c(1, 1), col=c("blue", "red"))
points(20, 0.6560425, col="blue")
text(30, 0.660, "20 Iterations", col="blue")
points(40, 0.6929686, col="red")
text(50, 0.69, "40 Iterations", col="red")
dev.off()
