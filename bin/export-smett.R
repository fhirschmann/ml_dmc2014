#!/usr/bin/env Rscript
source("R/data.R")
library(foreign)

noDD <- function(dt) {
    dt2 <- dt
    if (!is.null(dt$deliveryDateMissing)) {
        if (any(dt2$deliveryDateMissing == "yes"))
            dt2 <- dt2[dt2$deliveryDateMissing == "no", ]
    }
    dt2
}

write.arff(dt.train, "SMETT/train.arff")
write.arff(dt.test, "SMETT/test.arff")

write.arff(dt.dmc$M3$train, "SMETT/M3_train.arff")
write.arff(dt.dmc$M3$test, "SMETT/M3_test.arff")

write.arff(dt.dmc$M30$train, "SMETT/M30_train.arff")
write.arff(dt.dmc$M30$test, "SMETT/M30_test.arff")

write.arff(dt.dmc$M31$train, "SMETT/M31_train.arff")
write.arff(dt.dmc$M31$test, "SMETT/M31_test.arff")


write.arff(noDD(dt.train), "SMETT/noDDm_train.arff")
write.arff(noDD(dt.test), "SMETT/noDDm_test.arff")

write.arff(noDD(dt.dmc$M3$train), "SMETT/noDDm_M3_train.arff")
write.arff(noDD(dt.dmc$M3$test), "SMETT/noDDm_M3_test.arff")

write.arff(noDD(dt.dmc$M30$train), "SMETT/noDDm_M30_train.arff")
write.arff(noDD(dt.dmc$M30$test), "SMETT/noDDm_M30_test.arff")

write.arff(noDD(dt.dmc$M31$train), "SMETT/noDDm_M31_train.arff")
write.arff(noDD(dt.dmc$M31$test), "SMETT/noDDm_M31_test.arff")