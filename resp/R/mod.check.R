mod.check <- function(models, omit_NA=TRUE) {
    ## Check for fixed factors significance and unvalidated results
    ##    - adds new list 'check_out' to models
    if('mresp' %in% class(models)) {} else {stop('models object must be of class mresp')}
    require(stats, quietly = TRUE)

    response <- names(models)[- which(names(models) %in% c('ChosenModelResponse','UserDataFrame'))]
    mod_names <- names(models[[response[1]]])
    mod_names <- mod_names[- which(mod_names == 'void')]

    for (var in response) {
        all <- c()
        void <- c()
        sphere <- c()
        correl <- c()
        aic <- c()
        bic <- c()
        lglik <- c()
        for (m in mod_names) {

            # Models comparision by Chi squared
            chi_all = anova(models[[var]][[m]], models[[var]][['all']])$`Pr(>Chisq)`[2]
            all <- c(all, chi_all)
            chi_void = anova(models[[var]][[m]], models[[var]][['void']])$`Pr(>Chisq)`[2]
            void <- c(void, chi_void)

            # Check sphericity
            sph = shapiro.test(residuals(models[[var]][[m]]))
            sphere = c(sphere, sph$statistic)

            # Check correlation
            corr = cor.test(if (omit_NA) {
                na.omit(models[['UserDataFrame']][, var])
            } else {models[['UserDataFrame']][, var]},
                fitted(models[[var]][[m]]))
            correl <- c(correl, corr$estimate)

            # Retrieve AIC, BIC, and logLik
            aic <- c(aic, AIC(models[[var]][[m]]))
            bic <- c(bic, BIC(models[[var]][[m]]))
            lglik <- c(lglik, logLik(models[[var]][[m]])[1])

            
        }
        
        # Add result to response variable
        var_stats <- data.frame(
          Pr_vs_void = void,
          Pr_vs_all = all,
          Sphericity = sphere,
          Correlation = correl,
          AIC_ord = order(aic, decreasing = FALSE),
          BIC_ord = order(bic, decreasing = FALSE),
          logLik_ord = order(lglik, decreasing = TRUE)
          )
        row.names(var_stats) <- mod_names
        models[[var]][['check_out']] <- var_stats
    }
    return(models)
}


