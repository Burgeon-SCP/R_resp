print.mresp <-
function(x, printall=FALSE, ...){
  cat('[?] Response model evaluation:\n')

  if('ChosenModelResponse' %in% names(x) & printall) {
    vars <- names(x)[- which(names(x) == 'UserDataFrame')]
    vars <- vars[- which(vars == 'ChosenModelResponse')]
    for (i in vars){
      cat(i,':\n')
      a=x[[i]][['check_out']]
      cat('\t\t',paste(colnames(a),collapse = '\t'),'\n')

      # Print elements one by one
      for (i in 1:nrow(a)){
        cat(row.names(a)[i],'\t')
        if(row.names(a)[i]=='all' | row.names(a)[i]=='inter') cat('\t')
        for (j in 1:ncol(a)) {
          cat(round(a[i,j],3),
              # Add tabulation for ordened
              if(j %in% grep('_ord',names(a))){'\t'} else {'\t\t'}
          )}
        cat('\n')
      }
      cat('\n')
    }
  } else {
    print(x[['ChosenModelResponse']])
  }
}
