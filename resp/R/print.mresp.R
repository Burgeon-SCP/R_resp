print.mresp <-
function(x,...){
  cat('[?] Response model evaluation:\n')
  for (i in names(x)[- which(names(x) == 'UserDataFrame')]){
    cat(i,':\n')
    a=x[[i]][['check_out']]
    cat('\t\t',paste(colnames(a),collapse = '\t'),'\n')
    for (i in 1:nrow(a)){
      cat(row.names(a)[i],'\t')
      if(row.names(a)[i]=='all' | row.names(a)[i]=='inter') cat('\t')
      for (j in 1:ncol(a)) {cat(round(a[i,j],3),
                                if(j %in% grep('_ord',names(a))){'\t'} else {'\t\t'})
                                  }
      cat('\n')
    }
    cat('\n')
  }
}
