mod.choose <-  function(models, force=FALSE, sph_range=c(0.05,0.8), corr_range=c(0.4,0.8), pvalue_range=c(0.01,0.05),...) {

  ### Function to select model based on check_out table of each response variable for mresp models
  if('mresp' %in% class(models)) {} else {stop('models object must be of class mresp')}

  # #debug
  # cat('\n[debug] ChosenModelResponse values and str before choose:\n')
  # print(models[['ChosenModelResponse']])
  # str(models[['ChosenModelResponse']])

  # Inspect ChosenModelResponse object
  response <- models[['ChosenModelResponse']]
  if(is.vector(response)==FALSE & force==FALSE) {
    stop('[?] Chosen model data already present, try force=TRUE to reset results')
  }
  if(is.vector(response)==FALSE & force) {
    response <- response[,1L]
  }

  # Ensure that mresp object is checked
  for (i in response) {
    if(('check_out' %in% names(models[[i]]))==FALSE) {mod.check(models);break}
  }

  # Define output values
  mod_name <- c()
  sph_name <- c()
  corr_name <- c()
  f <- function(k) {c(mod_name,dimnames(k)[[1L]][1L])}

  for (var in response) {
    a <- list()
    x <- models[[var]][['check_out']]
    m <- row.names(x)

    # Categorize values
    for (st in names(x)) {
      if(grepl('Pr_',st)) {
        a[[st]] <- name_range(x[,st],pvalue_range); next
      } else if(st=='Sphericity') {
        a[[st]] <- name_range(x[,st],sph_range); next

      } else if(st=='Correlation') {
        a[[st]] <- name_range(x[,st],corr_range); next
      } else {
        a[[st]] <- x[,st]
      }
    }
    a <- as.data.frame(a, row.names=row.names(x))

    # P values comparision
    # Signigicant interaction
    if('inter' %in% m) {
      if(a['inter','Pr_vs_all']!='High') {
        mod_name <- c(mod_name,'inter'); next
      }
    }
    # No significance over void
    if(identical(a[,'Pr_vs_void'],rep('High',length(m)))) {
      mod_name <- c(mod_name,'void'); next
    }
    # Model selection
    mod <- as.matrix(a[grep('m_',m),grep('Pr_',names(x))])
    if(identical(as.vector(mod),rep('Low',length(mod)))) {
      mod_name <- c(mod_name,'all'); next
    }
    # To Be Removed
    if(identical(dim(mod),c(2L,2L))==FALSE) {
      stop('mod.choose still not implemented for more than 2 fixed factors')
    } else {
      # Choose between fixed factors
      rmod <- mod[2:1,]
      if(identical(diag(mod),c('Low','Low'))) {
        if(identical(diag(rmod),c('High','High'))) {
          mod_name <- f(mod); next
        } else {
          mod_name <- c(mod_name,
                        paste(dimnames(mod)[[1L]][1L],
                        if(diag(rmod)[1L]=='High') {'>>'} else {paste('>(',diag(rmod)[1L],')')},
                        dimnames(rmod)[[1L]][1L],
                        diag(rmod)[2L],
                        sep='')); next
        }

      } else if(identical(diag(rmod)==c('Low','Low'))) {
        if(identical(diag(mod),c('High','High'))) {
          mod_name <- f(rmod); next
        } else {
          mod_name <- c(mod_name,'ToDo2'); next
        }

      } else {mod_name <- c(mod_name,'ToDo::fail'); next}
    }

  }

  # Return result inside mresp object
  out <- data.frame(
    Variable=response,
    Model=mod_name#,
    # Sphericity=sph_name,
    # Correlation=cor_name
  )
  models[['ChosenModelResponse']] <- out
  return(models)
}



