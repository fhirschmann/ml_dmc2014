#!/usr/bin/env Rscript
#
# Installs the required dependencies

source("R/utils.R")

dmc.inst("-f" %in% c(commandArgs()))
