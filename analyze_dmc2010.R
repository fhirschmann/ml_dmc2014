# This is a special R script which can be used to generate a report. You can
# write normal text in roxygen comments.
# 
# Press CTRL + ALT + Shift + H in RStudio and select knitr::spin as Type.

#+ setup, include=FALSE
source("R/pp.R")
source("R/plot.R")
library(psych)
library(caret)

dt.test$target90 <- NA
cmb <- rbind(data.frame(dt, group="train"), data.frame(dt.test, group="test"))

#' Factor Plots

#' Discretize numeric
cmb2 <- cmb
nums <- colnames(cmb2[sapply(cmb2, is.numeric)])
cmb2[nums] <- lapply(cmb2[nums], as.factor)
facs <- colnames(cmb2[sapply(cmb2, is.factor)])

for (f in setdiff(facs, c("customernumber"))) {
    print(qplot(cmb2[[f]], data=cmb2, fill=cmb$group, geom="bar", position="dodge",
                xlab=f, ylab="count"))
}

#' Numeric Plots
#for (f in setdiff(nums, c("customernumber", "datecreated", "points",
#                          "deliverydatepromised", "deliverydatereal",
#                          dt_binary))) {
#    dmc.hist(dt[[f]], main=f)
#}

#' Correlation Plot
#+ corrplot, fig.width=10, fig.height=10
dmc.plotcorr(dt)