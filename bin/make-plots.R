#!/usr/bin/env Rscript
library(vcd)
source("R/data.R")

png("doc/pres_mosaic.png", width=8, height=6, units="in", res=300, bg="transparent")
mosaic(returnShipment ~ deliveryDateMissing, data=dt.train, shade=T, zero_size=1.0,
       main="Mosaic Plot of returnShipment vs deliveryDateMissing")
dev.off()