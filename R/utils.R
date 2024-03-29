# Utility functions

norm.lin <- function(x, mi=min(x), ma=max(x)) {
    (x - mi) / (ma - mi)
}

ht <- function(d, n=10) rbind(head(d, n), tail(d, n))

addlevels <- function (df1, df2) {
    # Adds factors levels from df2 to df1 and returns a new Data Frame.

    dfx <- df1
    facs <- names(sapply(df1, is.factor))
    for (fac in facs) {
        levels(dfx[[fac]]) <- c(levels(dfx[[fac]]), levels(df2[[fac]]))
    }
    dfx
}

table2 <- function(v1, v2, row.names=c("train", "test")) {
    # Frequency table for two factors with the same levels.
    #
    # Can be used for Chi-Sq test, e.g.:
    #   chisq.test(table2(dt$domain, dt.test$domain))
    #
    # Args:
    #   v1: first factor
    #   v2: second factor
    
    tbl <- rbind(table(v1), table(v2))
    rownames(tbl) <- row.names
    tbl
}

df.classes <- function(dt) {
    # Prints a Data Frame's column classes.
    #
    # Args:
    #   dt: a data frame
    
    data.frame(class=unlist(lapply(dt, function(x) paste(class(x), collapse="/"))))
}

list.update <- function(l1, l2) {
    # Updates list l1 with the contents of l2 and returns
    # the new list.
    #
    # Args:
    #   l1: a list
    #   l2: a list
    
    lr <- l1[setdiff(names(l1), names(l2))]
    c(lr, l2)
}

caret.bestidx <- function(fit) {
    # Returns the indices of the best tune observations and predictions
    # for a caret fit. Don't think about what this function does too hard...
    #
    # The predictions are saved in fit$pred$pred, but this vector contains
    # the predictions for *all* models across *all* tuning parameters, so this
    # function returns a logical vector with the indices for the predictions
    # for the best tune set to TRUE.
    #
    # Args:
    #   dt: a caret fit
    
    if (nrow(fit$results) == 1) {
        # Only one parameter configuration
        rep(TRUE, nrow(fit$pred))
    } else {
        params <- colnames(fit$bestTune)
        Reduce('&', lapply(params, function(y)
            as.character(fit$pred[[y]]) == as.character(fit$bestTune[[y]])))    
    }
}

caret.best <- function(fit, sort=F) {
    # Returns a Data Frame with predictions, observations,
    # and the fold for the best tune.
    #
    # Args:
    #   fit: a caret fit
    #   sort: sort to match the input data
    
    best <- fit$pred[caret.bestidx(fit),]
    
    if (sort) {
        best[order(best$rowIndex),]
    } else {
        best
    }
}

caret.pred <- function(fit) {
    # Extracts the predictions from a caret fit.
    #
    # Args:
    #   dt: a caret fit
    
    caret.best(fit, sort=T)$pred
}

caret.obs <- function(fit) {
    # Extracts the observations from a caret fit.
    #
    # Args:
    #   dt: a caret fit

    caret.best(fit, sort=T)$obs
}

caret.prob <- function(fit, fix.nas=NULL) {
    # Extracts the probabilty for each class from a caret fit.
    #
    # Args:
    #   dt: a caret fit
    #   fix.nas: fix NAs in class probabilities by using the given
    #            probabilities
    
    best <- caret.best(fit, sort=T)
    ret <- best[levels(fit$pred$pred)]
    nas <- any(sapply(levels(fit$pred$pred),
                      function(x) any(is.na(fit$pred[[x]]))))
    
    if (nas) {
        if (!is.null(fix.nas)) {
            ret[is.na(ret)] <- fix.nas
        } else {
            warning("Class probabilities contain NAs.")
        }
    }
    
    as.data.frame(ret)
}

caret.missidx <- function(fit) {
    # Returns the indices of the elements that were missclassified
    # 
    # Args:
    #   fit: a caret fit
    
    caret.pred(fit) != caret.obs(fit)
}

try.mp <- function() {
    # Enables multi-processing if available
    
    if ("doMC" %in% rownames(installed.packages())) {
        if ("parallel" %in% rownames(installed.packages())) {
            require(parallel)
            cores <- parallel::detectCores()
        } else {
            cores <- 4
        }
        
        require(doMC)
        doMC::registerDoMC(cores=cores)
        message("Enabled parallel processing with ", cores, " cores.")
    } else {
        message("Disabled parallel processing.")
    }
}

xtable <- function(x, ...) {
    require(xtable)

    # Bug in xtable: doesn't print dates correctly
    for (i in which(sapply(x, function(y) !all(is.na(match(c("POSIXt","Date"), 
                                                           class(y))))))) x[[i]] <- as.character(x[[i]])
    xtable::xtable(x, ...)
}