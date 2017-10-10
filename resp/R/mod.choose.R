mod.choose <-  function(models,sph_range=c(0.05,0.8),corr_range=c(0.4,0.8),pvalue_range=c(0.01,0.05),...) {
  ## Select model based on check_out table of each response variable
  if('mresp' %in% class(models)) {} else {stop('models object must be of class mresp')}
  response <- models[['ChosenModelResponse']]
  if(('check_out' %in% names(models[[response[1]]]))==FALSE) {mod.check(models)}
  named_range <- function(x,range,labels=c('Low','Mid','High')) {
    # Chose label based on range as [Low]|(Mid]_n|(High]
    range = sort(range)
    if(x<=range[1]) {return(labels[1])} else if(x>range[length(range)]) {return(labels[3])} else {
      if(length(range)==2) {return(labels[2])} else {
        mid <- range[-c(1,length(range))]
        for (i in mid) {
          if(x<=i) {return(paste(labels[2],'_',mid[which(mid=i)]))}
        }
      }
    }
  }
  mod_name <- c()
  sph_name <- c()
  corr_name <- c()
  
  for (i in response) {
    # P values comparision
    x <- models[[i]][['check_out']]
    n <- row.names(x)
    if('inter' %in% n) {
      if(named_range(x['inter','Pr_vs_all'],pvalue_range)!='High') {mod_name <- c(mod_name,'inter');break} 
    }
    if(named_range(x['all','Pr_vs_void'],pvalue_range)=='High' & 
       named_range(x[2,'Pr_vs_void'],pvalue_range)=='High' &
       named_range(x[3,'Pr_vs_void'],pvalue_range)=='High') {mod_name <- c(mod_name,'void');break}

    m <- c()
    all <- c()
    void <- c()
    for (j in n[grep('m_',n)]) {
      m <- c(m,j)
      all <- c(all,named_range(x[j,'Pr_vs_all'],pvalue_range))
      void <- c(void,named_range(x[j,'Pr_vs_void'],pvalue_range))
    }
    if(length(m)==2) {
      if(all[1]==void[2] & all[2]==void[1]) {
        if(all[1]=='High') {mod_name <- c(mod_name,m[2]);break} else {mod_name <- c(mod_name,m[1]);break}
      }
    } else {
      Stop('mod.choose still not implemented for more than 2 fixed factors')
      # for (j in m) {
      #   if(all[j]=='High' & void[j]!='High')
      # }
      # 
    }
  }
  
  
  out <- data.frame(
    Variable=response,
    Model=mod_name#,
    # Sphericity=sph_name,
    # Correlation=cor_name
    )
  # Return result inside mresp object
  models[['ChosenModelResponse']] <- out
  return(models)
}



