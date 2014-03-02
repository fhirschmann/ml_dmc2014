# This is a special R script which can be used to generate a report. You can
# write normal text in roxygen comments.
# 
# Press CTRL + ALT + Shift + H in RStudio and select knitr::spin as Type.

#+ setup, include=FALSE
source("R/pp.R")
source("R/plot.R")
library(psych)
library(caret)
nums <- colnames(dt[sapply(dt, is.numeric)])

#' Describe Data
#+ desc, cache=TRUE
prettyR::describe(dt)

#' Factor Plots
for (f in setdiff(dt_factors, c("customernumber"))) {
    plot(dt[[f]], main=f)
}

#' Binary Factor Plots
for (f in dt_binary) {
    plot(dt[[f]], main=f)
}

#' Numeric Plots
for (f in setdiff(nums, c("customernumber", "datecreated", "points",
                          "deliverydatepromised", "deliverydatereal",
                          dt_binary))) {
    dmc.hist(dt[[f]], main=f)
}

#' Correlation Plot
#+ corrplot, fig.width=10, fig.height=10
dmc.plotcorr(dt)