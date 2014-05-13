#!/usr/bin/env Rscript
library(plyr)
library(stringr)

test <- read.table("orders_class.txt", sep=";", header=T)

files <- list.files(pattern="Final.*.txt")
n <- str_replace(files, "Final_group", "G")
n <- str_replace(n, "_confidence.txt", "_C")
n <- str_replace(n, "_binary.txt", "_B")
n <- str_replace(n, "Final_", "")
n <- str_replace(n, "ensemblePredictions", "E")
n <- str_replace(n, "randomforest", "rf")

cat("Name Map:\n")
data.frame(File=files, Name=n)

preds <- sapply(files, function(x) read.table(x, sep=";", header=T), simplify=F)
merged <- Reduce(function(a, p) join(a, p, by="orderItemID"), preds, data.frame(orderItemID=1:50078))
colnames(merged) <- c("orderItemID", n)

agree <- function(a, b) ifelse(a >= 0.5, 1, 0) == ifelse(b >= 0.5, 1, 0)

diff <- sapply(n, function(a) sapply(n, function(b) sum(agree(merged[[a]], merged[[b]]))))
diff2 <- diff / nrow(merged) * 100

cat("\n\nAbsolute Agreements\n")
diff

cat("\n\nRelative Agreements (in %)\n")
round(diff2, 0)