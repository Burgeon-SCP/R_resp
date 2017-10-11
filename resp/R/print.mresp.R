print.mresp <-
function(x, printall=FALSE, ...){
  cat('[?] Response model evaluation:\n')

  if(printall) {
    vars <- names(x)[- which(names(x) == 'UserDataFrame')]
    vars <- vars[- which(vars == 'ChosenModelResponse')]
    for (i in vars){
      cat(i,':\n')
      a=x[[i]][['check_out']]
      n=colnames(a)[-which(colnames(a) %in% c('AIC','BIC'))]

      # Print column names except AIC and BIC
      cat('\t\t', paste(n, collapse = '\t'), '\n')

      # Print elements one by one
      for (i in 1:nrow(a)){
        cat(row.names(a)[i],'\t')
        if(row.names(a)[i]=='all' | row.names(a)[i]=='inter') cat('\t')

        for (j in n) {
          # Skip AIC and BIC values
          if(j %in% c('AIC','BIC')){next}
          cat(round(a[i,j],3),'\t\t')}
        cat('\n')
      }
      cat('\n')
    }
  } else {
    print(x[['ChosenModelResponse']])
  }
}
