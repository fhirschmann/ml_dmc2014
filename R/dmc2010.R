# DMC2010 Specific Stuff

suppressPackageStartupMessages(library(caret))


dmc.points <- function(pred, obs) {
    voucher <- ifelse(pred == "yes", "no", "yes")
    sum(ifelse(voucher == "yes",
               ifelse(obs == "yes", -5, 1.5),
               0))
}

dmc.summary <- function (data, lev = NULL, model = NULL) {
    out <- twoClassSummary(data, lev, model)
    c(Points=dmc.points(data$pred, data$obs), out)
}

dmc.cost <- matrix(c(0, 5, 1.5, 0), 2, 2, byrow=TRUE)
colnames(dmc.cost) <- rownames(dmc.cost) <- rev(c("no", "yes"))
