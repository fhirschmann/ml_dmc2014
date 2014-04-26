#!/usr/bin/env Rscript

source("R/dmc.R")
args <- commandArgs(T)

load(args[[1]])
exportPreds.DmcTrain(fit, args[[2]])