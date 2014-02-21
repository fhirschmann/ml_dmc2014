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

# Usage: dmc.hist(dt$weight)
dmc.hist <- function(dat, ...) {
    ds <- rbind(data.frame(dat=dat, grp="All"))
    hs <- hist(dat, border=TRUE, ...)
    dens <- density(dat, na.rm=TRUE)
    rs <- max(hs$counts)/max(dens$y)
    lines(dens$x, dens$y*rs, col="red")
    legend("topright",legend=c("Histogram","Kernel Density Estimate"),lty=c(NA,1),
           lwd=c(NA,2), pch = c(15, NA), col = c("grey80", "red"), merge=TRUE, 
           inset=.01, cex=.8, adj=0)
    rug(ds[ds$grp=="All",1])
}
