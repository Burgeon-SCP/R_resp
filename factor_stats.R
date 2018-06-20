#!/usr/bin/R

# Check data subsets to optain basic statistics to be plotted or modeled

factor_stats <- function(dataset, factors = c("Specie", "Treatment"), response = NULL, 
    melting = list(TRUE)) {
    #'''Convert dataframe to a list of 2D tables of basic statistics from subseted responses'''
    "
    Factorized analysis on a dataset for each response variable
       - If response parameter not given, all remaining columns are analyzed
       - Returns data.frame if melting=TRUE (DEFAULT)
       - Returns list of tables, one for each response variables
  "
    
    require(stats, quietly = T)
    if (!is.list(melting)) {
        if (melting) {
            melting <- list(TRUE)
            require(reshape2, quietly = T)
        } else if (!melting) {
            melting <- list(FALSE)
        } else {
            stop("melting must be TRUE or FALSE")
        }
    } else {
        if (melting[[1L]]) {
            require(reshape2, quietly = T)
        }
    }
    
    if (!identical(factors %in% colnames(dataset), rep(TRUE, length(factors)))) {
        stop("factors must be column names of dataset")
    }
    
    if (length(factors) >= 3L) {
        stop("More than 2 factors still not implemented, sorry!")
    }
    
    if (is.null(response)) {
        factors_c <- which(colnames(dataset) %in% factors)
        response <- colnames(dataset[, -factors_c])
        # cat('Using response variables:\n') print(response)
        if (length(response) == 0L) {
            stop("No remaining response variables in dataset")
        }
    } else if (!is.vector(response)) {
        stop("If given, response must be a character vector of column names")
    } else {
        if (!identical(response %in% colnames(dataset), rep(TRUE, length(response)))) {
            stop("Response names not found as dataset columns")
        }
    }
    
    pos <- function(i, j, c = cases) {
        x1 = which(c[[1L]] == i)
        x2 = which(c[[2L]] == j)
        col = x2 - 1L
        len = length(c[[1L]])
        res = as.numeric((col * len) + x1)
        return(res)
    }
    
    calc <- function(res_var, data = dt, fact = factors, cases_l = cases) {
        # @TODO: extend l table generation to n-dimensional based on cases list
        
        index_f <- list()
        for (f in 1:length(fact)) {
            index_f[[f]] <- data[, fact[f]]
        }
        l <- tapply(data[, res_var], INDEX = index_f, list)
        
        # Create 'empty' tables
        t <- table(data[, fact])
        low <- t - t
        m <- t - t
        top <- t - t
        s <- t - t
        p <- t - t
        normal <- t - t
        n <- t - t
        
        # cat('[debug]: initial objects definitions');print(l);print(t)
        
        # Calculate results
        for (i in cases_l[[1L]]) {
            for (j in cases_l[[2L]]) {
                g <- as.numeric(as.character(l[[pos(i, j)]]))
                # log transform g[which(g!=0)] <- log(g[which(g!=0)])
                s[i, j] <- sum(g)
                n[i, j] <- length(g)
                
                if (identical(g, rep(g[1L], length(g)))) {
                  low[i, j] <- g[1L]
                  m[i, j] <- g[1L]
                  top[i, j] <- g[1L]
                  p[i, j] <- 0L
                  normal[i, j] <- NA
                } else {
                  try(x <- t.test(g), TRUE)
                  if (!exists("x")) {
                    low[i, j] <- g[1L]
                    m[i, j] <- g[1L]
                    top[i, j] <- g[1L]
                    p[i, j] <- 0L
                  } else {
                    low[i, j] <- x$conf.int[1L]
                    m[i, j] <- x$estimate
                    top[i, j] <- x$conf.int[2L]
                    p[i, j] <- x$p.value
                  }
                  
                  try(sha <- shapiro.test(g), TRUE)
                  if (!exists("sha")) {
                    normal[i, j] <- NA
                  } else {
                    normal[i, j] <- sha$p.value
                  }
                }
            }
        }
        # Return rounded values TODO: Adapt to dataset decimals
        out <- list(low = round(low, 2L), mean = round(m, 2L), top = round(top, 2L), 
            n = n, p.value = p, sum = s, SW_norm = normal)
    }
    
    melted <- function(x = out) {
        melted <- melt(x[[1L]])
        melted$value <- NULL
        for (i in names(x)) {
            melted[, i] <- melt(x[[i]])$value
        }
        return(melted)
    }
    
    response <- as.character(response)
    single = FALSE
    if (length(response) == 1L) {
        single = TRUE
    }
    
    dt <- dataset[, c(factors, response)]
    cases <- list()
    for (j in factors) {
        dt[, j] <- factor(dt[, j])
        cases[[j]] <- levels(as.factor(dt[, j]))
    }
    # cat('\nResulting dataset:\n') print(head(dt)) cat('\nResulting factor
    # levels:\n') print(cases)
    
    # Calculate response statistics
    if (single) {
        out <- calc(response)
    } else {
        out <- list()
        for (i in response) {
            out[[i]] <- calc(i)
            if (is.null(out[[i]])) {
                out[[i]] <- "constant data"
            }
        }
    }
    
    # melt [and drop] if melted=TRUE [value,sign,column]
    if (melting[[1L]] == TRUE) {
        if (single) {
            out <- melted(out)
        } else {
            m_out <- melted(out[[1L]])
            m_out$response <- as.character(m_out[, 1L])
            m_out[1L, ] <- rep(NA, ncol(m_out))
            m_out <- m_out[1L, ]
            print(names(out))
            for (i in names(out)) {
                m_res <- melted(out[[i]])
                m_res$response <- as.character(rep(i, nrow(m_res)))
                m_out[(nrow(m_out) + 1):(nrow(m_out) + nrow(m_res)), ] <- m_res
            }
            m_out <- m_out[-1L, ]
            out <- m_out
        }
        # if (is.integer(melting[[2]]) | is.numeric(melting[[2]])) { #@TODO: ADD options
        # to melting argument to incorporate c(<,<=,==,>=,>,%in%) melted <-
        # melted[-which(melted[,melting[[3]]]==melting[[2]]),] out <- melted }
    }
    
    # Return final outcome
    return(out)
}
