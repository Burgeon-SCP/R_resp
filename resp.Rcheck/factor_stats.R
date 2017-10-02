#/usr/bin/R
## Check data subsets to obtain basic stadistics to be plotted or modeled

factor.stats <- function(
  dataset,
  factors=c('Specie','Treatment'),
  element, # Stripped to 1 element
  melting=list(TRUE)#,0,'sum') # list(ON/OFF [,drop_value,@TODO:drop_sign, drop_column_to_check])
) {
  #"""Convert dataframe to a list of 2D tables of basic statistics from subseted elements"""
  """Factorized analysis on a dataset based on discrete independent variables

    returns dataframe
  """
  
  require(stats)  
  require(reshape2)
  
  
  element <- element[1]
  cols = c(factors,element)
  dt <- dataset[,c(factors,element)]
  
  # # @TODO: Check columns for transposed matrix names
  # for (i in cols) {
  #   if (identical((cols %in% names(dt)),rep(FALSE,length(names(dt))))) {
  #     if (cols %in% names(t(dataset))) {dataset<-t(dataset)} else {
  #       stop("[!] Defined factors doesn't exist in dataset names attribute")
  #     }
  #   }
  # }
  
  for (j in factors) {dt[,j] <- factor(dt[,j])}
  t <- table(dt[,factors])
  cases <- list()
  for (i in factors) {
    cases[[i]] <- levels(as.factor(dt[,i]))
  }
  
  # @TODO: extend l table generation to n-dimensional based on cases list  
  l <- tapply(
    dt[,element],
    INDEX=list(dt[,factors[1]],dt[,factors[2]]),
    list
  )
  
  pos <- function(i,j,c=cases) {
    x1 = which(c[[1]]==i)
    x2 = which(c[[2]]==j)
    col = x2-1
    len = length(c[[1]])
    res = as.numeric((col*len)+x1)
    return(res)
  }
  
  # Create "empty" tables
  low <- t-t
  m <- t-t
  top <- t-t
  s <- t-t
  p <- t-t
  normal <- t-t
  
  
  # Calculate results
  for (i in cases[[1]]) {
    for (j in cases[[2]]) {
        g <- l[[pos(i,j)]]
        # log transform
        # g[which(g!=0)] <- log(g[which(g!=0)])

        if (identical(g,rep(g[1],length(g)))) {
          low[i,j] <- g[1]
          m[i,j] <- g[1]
          top[i,j] <- g[1]
          p[i,j] <- 0
          s[i,j] <- sum(g)
          normal[i,j] <- 0
        } else {
          x <- t.test(g)
          sha <- shapiro.test(g)
          low[i,j] <- x$conf.int[1]
          m[i,j] <- x$estimate
          top[i,j] <- x$conf.int[2]
          p[i,j] <- x$p.value
          s[i,j] <- sum(l[[pos(i,j)]])
          normal[i,j] <- sha$p.value
        }
        
    }
  }
  # Return rounded values
  # TODO: Adapt to dataset decimals
  out <- list(
    low=round(low,2),
    mean=round(m,2),
    top=round(top,2),
    n=t,
    p.value=p,
    sum=s,
    SW_norm=normal
    )

    # melt [and drop] if melted=TRUE [value,sign,column]
  if (melting[[1]]==TRUE) {
    melted <- melt(out[[1]])
    melted$value <- NULL
    for (i in names(out)) {
      melted[,i] <- melt(out[[i]])$value
    }
    out <- melted
  #   if (is.integer(melting[[2]]) | is.numeric(melting[[2]])) {
  #     #@TODO: ADD options to melting argument to incorporate c(<,<=,==,>=,>,%in%)
  #     melted <- melted[-which(melted[,melting[[3]]]==melting[[2]]),]
  #     out <- melted
  #   } 
  }
   
  # Return final outcome
  return(out)
}
