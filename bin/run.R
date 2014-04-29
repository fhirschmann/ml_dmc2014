#!/usr/bin/env Rscript
#
# This runs the pipeline.
#
# You can give specify the models to train by giving
# its names as arguments, e.g. ./bin/run.R c50 rf
#
# To get a list of available models try ./bin/run.R -l

source("R/data.R")
source("R/dmc.R")
source("R/pipeline.R")

args <- commandArgs(T)

strain <- dmcstrain(descs, common.desc, train.only=args)