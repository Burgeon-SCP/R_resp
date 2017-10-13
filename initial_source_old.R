
## Modeling functions after nlme package
# s <- function(x){return(summary(x))}
# Edu_R_all_variables <- read.csv("/disk2/Documentos/Riparian/rizopod_ipe/R/Edu_R_all_variables.csv", header=TRUE)

# Get biomass measures but exclude ratio and include LeaveArea
# bio <- names(Edu_R_all_variables)[which(grepl('Bio',names(Edu_R_all_variables)))]
# bio <- bio[-which(bio=='ShootToRootBio')]
# bio <- c(bio,'LeavesArea')

log.dataset <- function(data,columns=1:dim(data)[2]) {
  # Create log values for biomass without 0 | NA values to avoid -Inf | NaN
  for (i in columns) {
    if(is.factor(data[,i]) | is.character(data[,i])) {next}
    x <- data[,i]
    for (j in 1:length(x)){
      if (is.na(x[j])) {next}
      if (x[j]==0) {next}
      x[j]<-log(x[j])
    }
    data[,i] <- x
  }
  return(data)
}

# Edu_log <- log.dataset(Edu_R_all_variables, bio)

mod.resp <- function(data, fixed, random, r_group,
                           exclude, omit_NA=TRUE,
                           fixed_interaction=TRUE,
                           check_models=TRUE
                           ) {
 
  require(lme4, quietly = TRUE)
  require(stats, quietly = TRUE)
  # require(nlme, quietly = TRUE)
  ### !!!!! 
  # nlme::formula.lme must be fixed to formula(terms(x))
  # crtl <- lmeControl(opt = lme_cntrl,maxIter = max_iter,msMaxIter = max_iter)
 
  # Formula components creator
  formula_from_vec <- function(x,start='~ ',mid=' + ',end='', as_formula=FALSE) {
    if (length(x)>0) {a=paste(start,x[1],sep='')}
    else {stop('[!] Empty vector')}
    if(length(x)>1) {
      for (i in 2:length(x)) {
        a <- paste(a,x[i],sep=mid)
      }
    }
    a=paste(a,end,sep='')
    if(as_formula) {
      return(as.formula(a))
    } else {
      return(a)
    }
  }

  # Modelling funtion
  mod.form <- function(dataset = dt,...) {return(lmer(data = dataset,...))}
  # Create model
  # model_args = list(fixed = as.formula(paste(var,fixed_formula)),
  #                   random = rand_formula,
  #                   method = "ML",
  #                   control = crtl)
  
  
  # Models comparision
  mod.check <- function(models, data=data, omit_NA=TRUE) {
    ## Check for fixed factors significance and unvalidated results
    ##    - adds new list 'check_out' to models
    require(stats, quietly = TRUE)
    mod_name <- names(models[[1]])
    # mod_name <- mod_name[-which(mod_name=='all')]
    mod_name <- mod_name[-which(mod_name=='void')]
    
    for (var in names(models)) {
      all <- c()
      void <- c()
      sphere <- c()
      correl <- c()
      aic <- c()
      bic <- c()
      lglik <- c()
      for (m in mod_name) {
        
        # Models comparision by Chi squared
        chi_all = anova(models[[var]][[m]],models[[var]][['all']])$`Pr(>Chisq)`[2]
        all <- c(all,chi_all)
        chi_void = anova(models[[var]][[m]],models[[var]][['void']])$`Pr(>Chisq)`[2]
        void <- c(void,chi_void)
        
        # Check sphericity
        sph = shapiro.test(residuals(models[[var]][[m]]))
        sphere = c(sphere,sph$statistic)
        
        # Check correlation
        corr = cor.test(if(omit_NA){na.omit(data[,var])} else {data[,var]},
                        fitted(models[[var]][[m]]))
        correl <- c(correl,corr$estimate)
        
        # Retrieve AIC, BIC, and logLik
        aic <- c(aic,AIC(models[[var]][[m]]))
        bic <- c(bic,BIC(models[[var]][[m]]))
        lglik <- c(lglik,logLik(models[[var]][[m]])[1])
        
        # Add result to response variable
      }
      var_stats <- data.frame(Pr_vs_void=void,
                              Pr_vs_all=all,
                              Sphericity=sphere,
                              Correlation=correl,
                              AIC_ord=order(aic,decreasing = FALSE),
                              BIC_ord=order(bic,decreasing = FALSE),
                              logLik_ord=order(lglik,decreasing = TRUE))
      row.names(var_stats) <- mod_name
      models[[var]][['check_out']] <- var_stats
    }
    return(models)
  }
  
  # Set objects and parameters
  out=list()
  const_dt=c(fixed,random,r_group)
  response=colnames(data)[-which(colnames(data) %in% c(fixed,random,r_group,exclude))]
  if(fixed_interaction & length(fixed)==1) {
    cat('[-] No interaction to compute due to single fixed factor\n')
    fixed_interaction = FALSE
  }
  
  cat('[?] Starting modelization on ',length(response),' response elements',fill = TRUE)
  cat('[+] Data row number: ',nrow(data),fill = TRUE)
  
  # Fixed factors formula
  fixed_formula <- formula_from_vec(x=fixed)
  if(fixed_interaction) {
    if(length(fixed)==2) {
      interact <- formula_from_vec(x=fixed,
                                        start = ' + ',
                                        mid = ' * ')
      inter_formula <- paste(fixed_formula,interact,sep = '')
    } else {
      cat('[-] Interaction method only supported for two fixed factors at this time.\n')
    }
  }
  
  # Random factors formula
  if(length(random)==1 & length(r_group)==1) {
    rand_formula = paste('(1+',random,'|',r_group,')')
  } else {
    cat('[-] Random formula only supports one random and one grouping at this time.',fill = TRUE)
    # if(length(r_group)>1) {
    #   rand_group <- formula_from_vec(r_group,start = '', mid = ' | ')
    # } else {
    #   rand_group = r_group
    # }
    # rand_unit = c()
    # if(random_unique) {
    #   for (i in 1:length(random)) {
    #     rand_unit[i] <- formula_from_vec(x=random[i], start = '(', end = ')')
    #   }
    #   rand_final <- formula_from_vec(x=rand_unit,start = '+ ')
    # } else {
    #   if(length(random>1)) {
    #     rand_final <- formula_from_vec(x=random, start = ' | ', mid = ' | ')
    #   } else {rand_final <- random}
    # }
    # rand_formula = paste(rand_final, if(length(r_group)>0) {' | '} else {''}, rand_group,sep = '')
  }
  
  # Modelling loop
  cat('[+] Modeling response variables to :\t',paste(fixed_formula,rand_formula),'\n')
  # if(fixed_interaction) {cat('[+] Modelling interaction to:\t',paste(inter_formula,rand_formula),'\n')}
  for (var in response) {
    # TODO: ensure rand_formula not null or with empty values after symbol
    cat('\t',which(response==var),'/',length(response),' ',var,sep = '')
    if(omit_NA) {
      dt=na.omit(data[,c(var,const_dt)])
      cat(': omit_NA n:',nrow(dt))
    }
    if(!exists('dt')) {dt<-data[,c(var,const_dt)]}
    
    model_void = list(formula = as.formula(paste(var,'~ ',rand_formula)),REML = FALSE)
    model_fixed = c()
    for(i in fixed) {
      n = paste('m_',i,sep = '')
      m = list(formula = as.formula(paste(var,'~ ',i,' + ',rand_formula)),REML = FALSE)
      assign(n,m)
      model_fixed = c(model_fixed,n)
    }
    model_all = list(formula = as.formula(paste(var,fixed_formula,' + ',rand_formula)),
                      REML = FALSE)
    if(fixed_interaction) {
      model_inter = list(formula = as.formula(paste(var,inter_formula,' + ',rand_formula)),
                              REML = FALSE)
    }
  
    
    out[[var]] <- list()
    out[[var]][['void']] = do.call('mod.form', model_void)
    out[[var]][['all']]=do.call('mod.form', model_all)
    for (m in model_fixed) {
      out[[var]][[m]]=do.call('mod.form',get(m))
    }
    if(fixed_interaction) {
      out[[var]][['inter']]=do.call('mod.form', model_inter)
    }

    cat('\t\tdone\n')
  }

  if(check_models) {
    out <- mod.check(out, data, omit_NA=omit_NA)
  }
  class(out) <- append(class(out),"mresp")
  return(out)
}

# check <- function(x,...) {UseMethod('check',x)}

print.mresp <- function(x){
  cat('[?] Response model evaluation:\n')
  for (i in names(x)){
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

# Create models
# resp <- mod.resp(data = Edu_log,
#                         fixed = c('Species', 'Treatment'),
#                         random = c('CuttingBio'),
#                         r_group = c('Tank'),
#                         exclude = c('Species_ID','CuttingLength'))
# 
# # Print results
# print(resp)
# 
# # Model comparision results list
# str(s(resp[[1]][['all']]))
# 
# 
# 
# # ALL RESPONSE VARIABLES HAVE Treatment AND Species AS SIGNIFICANT FIXED VARIABLE
# # RESPONSE VARIABLES WITHOUT SIGNIFICANT INTERACTION Species*Treatment
# no_interaction <- c('RootBio', 'StemLength', 'StemBio',
#                     'LeavesBio', 'TotalAboveBio', 'RootBio20.40')
# 
# 
# # a = lmer('RootBio ~ (CuttingBio|Tank)',data = Edu_log, REML = FALSE)
