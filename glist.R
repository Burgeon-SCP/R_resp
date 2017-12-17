# Title     : TODO
# Objective : TODO
# Created by: adria
# Created on: 31/10/17

pdev <- function(x,factors) {

    dt <- as.data.frame(x)
    for (j in factors) {dt[, j] <- factor(dt[, j])}
    t <- table(dt[, factors])
    cases <- list()
    for (i in factors) {
        cases[[i]] <- levels(as.factor(dt[, i]))
    }

    # @TODO: extend l table generation to n-dimensional
    l <- tapply(
    dt[,],
    INDEX = list(dt[, factors[1]], dt[, factors[2]]),
    list
    )

    pos <- function(i, j, c=cases) {
        x1 = which(c[[1]] == i)
        x2 = which(c[[2]] == j)
        col = x2 - 1
        len = length(c[[1]])
        res = as.numeric((col * len) + x1)
        return(res)
    }

    # Create "empty" tables
    low <- t - t
    m <- t - t
    top <- t - t
    s <- t - t
    p <- t - t
    normal <- t - t


    # Calculate results
}