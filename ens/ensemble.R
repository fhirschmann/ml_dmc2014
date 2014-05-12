#!/usr/bin/env Rscript
source("R/data.R")
library(data.table)
library(plyr)
library(doMC)
library(caret)
registerDoMC(cores = 4)



ens <- list(
    M30=list(
        c50=read.table("ens/c50C_M30_pred.txt", sep=";", header=T, col.names=c("orderItemID", "c50")),
        pda=read.table("ens/pdaC_M30_prob.txt", sep=";", header=T, col.names=c("orderItemID", "pda")),
        gbm=read.table("ens/gbmCs5_M30_prob.txt", sep=";", header=T, col.names=c("orderItemID", "gbm"))
    ),
    M31=list(
        
    )
)

m30 <- join(join(join(ens$M30$c50, ens$M30$pda), ens$M30$gbm), dt.dmc$M30$test)
#m30$returnShipment <- revalue(m30$returnShipment, c("yes"=0, "no"=1))
m30.orderItemID <- m30$orderItemIDachs
m30$orderItemID <- NULL

fit <- train(returnShipment ~ c50 + gbm + pda, data=m30, method="svmPoly")
fit