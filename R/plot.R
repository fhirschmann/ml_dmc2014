### Plotting Functions ###
source("R/lib/plotcorr.R")
source("R/pp.R")

# Usage: dmc.plotcorr(dt)
dmc.plotcorr <- function(dt) {
    dt.numeric <- dt[sapply(dt, is.numeric)]
    colsc <- c(rgb(241, 54, 23, maxColorValue=255), 
               'white', rgb(0, 61, 104, maxColorValue=255))
    colramp <- colorRampPalette(colsc, space='Lab')
    corr <- cor(dt.numeric,
                use="all.obs", method="pearson")
    colors = colramp(100)
    my.plotcorr(corr, col=colors[((corr + 1)/2) * 100],
                diag='ellipse', upper.panel="number", mar=c(0,2,0,0))
}