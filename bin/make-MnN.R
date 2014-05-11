#!/usr/bin/env Rscript

source("R/data.R")

m1.known <- dt.dmc$M1$test$customerID %in% unique(dt.dmc$M1$train$customerID)
m2.known <- dt.dmc$M2$test$customerID %in% unique(dt.dmc$M2$train$customerID)
m3.known <- dt.dmc$M3$test$customerID %in% unique(dt.dmc$M3$train$customerID)
test.known <- dt.test$customerID %in% unique(dt.train$customerID)

message(paste("Known in M1:", sum(m1.known) / length(m1.known)))
message(paste("Known in M2:", sum(m2.known) / length(m2.known)))
message(paste("Known in M3:", sum(m3.known) / length(m3.known)))
message(paste("Known in all Data:", sum(test.known) / length(test.known)))

dt.dmc$M1$test <- data.frame(dt.dmc$M1$test)
dt.dmc$M2$test <- data.frame(dt.dmc$M2$test)
dt.dmc$M3$test <- data.frame(dt.dmc$M3$test)
dt.test <- data.frame(dt.test)

write.csv(dt.dmc$M1$test[m1.known, c("orderItemID"), drop=F], file="eva/M11_test.txt", row.names=F)
write.csv(dt.dmc$M1$test[!m1.known, c("orderItemID"), drop=F], file="eva/M10_test.txt", row.names=F)

write.csv(dt.dmc$M2$test[m2.known, c("orderItemID"), drop=F], file="eva/M21_test.txt", row.names=F)
write.csv(dt.dmc$M2$test[!m2.known, c("orderItemID"), drop=F], file="eva/M20_test.txt", row.names=F)

write.csv(dt.dmc$M3$test[m3.known, c("orderItemID"), drop=F], file="eva/M31_test.txt", row.names=F)
write.csv(dt.dmc$M3$test[!m3.known, c("orderItemID"), drop=F], file="eva/M30_test.txt", row.names=F)

write.csv(dt.test[test.known, c("orderItemID"), drop=F], file="eva/all1_test.txt", row.names=F)
write.csv(dt.test[!test.known, c("orderItemID"), drop=F], file="eva/all0_test.txt", row.names=F)
