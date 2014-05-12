#!/usr/bin/env Rscript

source("R/data.R")


m3.known <- dt.dmc$M3$test$customerID %in% unique(dt.dmc$M3$train$customerID)


message(paste("Known in M3:", sum(m3.known) / length(m3.known)))


dt.dmc$M3$test <- data.frame(dt.dmc$M3$test)
dt.test <- data.frame(dt.test)

write.csv(dt.dmc$M3$test[m3.known, c("orderItemID"), drop=F], file="eva/M31_test.txt", row.names=F)
write.csv(dt.dmc$M3$test[!m3.known, c("orderItemID"), drop=F], file="eva/M30_test.txt", row.names=F)