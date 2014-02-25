# DMC2010 Specific Stuff

dmc.points <- function(pred, obs) {
    voucher <- ifelse(pred == "yes", "no", "yes")
    sum(ifelse(obs == "yes",
               ifelse(voucher == "yes", -5, 1.5),
               0))
}

dmc.summary <- function(data, lev=NULL, model=NULL) {
    out <- c(dmc.points(data$pred, data$obs))
    names(out) <- c("Points")
    out
}