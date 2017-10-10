mod.resp <-
function(data, fixed, random, r_group,
        exclude, omit_NA=TRUE,
        fixed_interaction=TRUE,
        check_models=TRUE,
        choose_models=TRUE) {

    require(lme4, quietly = TRUE)
    # require(nlme, quietly = TRUE)
    ### !!!!!
    # nlme::formula.lme must be fixed to formula(terms(x))
    # crtl <- lmeControl(opt = lme_cntrl,maxIter = max_iter,msMaxIter = max_iter)

    # Modelling funtion
    mod.form <- function(dataset = dt, ...) {return(lmer(data = dataset, ...))}
    # Create model
    # model_args = list(fixed = as.formula(paste(var,fixed_formula)),
    #                   random = rand_formula,
    #                   method = "ML",
    #                   control = crtl)


      # Set objects and parameters
    const_dt = c(fixed, random, r_group)
    response = colnames(data)[- which(colnames(data) %in% c(fixed, random, r_group, exclude))]
    if('UserDataFrame' %in% response) {
        stop('UserDataFrame is a reserved label of mresp list, please change it.')
    }
    if('ChosenModelResponse' %in% response) {
        stop('ChosenModelResponse is a reserved label of mresp list, please change it.')
    } 
    if (fixed_interaction & length(fixed) == 1) {
        cat('[-] No interaction to compute due to single fixed factor\n')
        fixed_interaction = FALSE
    }

    out = list()
    out[['UserDataFrame']] <- data
    out[['ChosenModelResponse']] <- response

    # #debug
    # cat('\n\n[debug] initial object names:',names(out),'\n\n')
    # cat('[debug] initial object str:\n')
    # print(str(out))
    # print((out)[[2]])

    cat('[?] Starting modelization on ', length(response), ' response elements', fill = TRUE)
    cat('[+] Data row number: ', nrow(data), fill = TRUE)

    # Fixed factors formula
    fixed_formula <- formula_from_vec(x = fixed)
    if (fixed_interaction) {
        if (length(fixed) == 2) {
            interact <- formula_from_vec(x = fixed,
            start = ' + ',
            mid = ' * ')
            inter_formula <- paste(fixed_formula, interact, sep = '')
        } else {
            cat('[-] Interaction method only supported for two fixed factors at this time.\n')
        }
    }

    # Random factors formula
    if (length(random) == 1 & length(r_group) == 1) {
        rand_formula = paste('(1+', random, '|', r_group, ')')
    } else {
        cat('[-] Random formula only supports one random and one grouping at this time.', fill = TRUE)
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
    cat('[+] Modeling response variables to :\t', paste(fixed_formula, rand_formula), '\n')
    # if(fixed_interaction) {cat('[+] Modelling interaction to:\t',paste(inter_formula,rand_formula),'\n')}
    for (var in response) {
        # TODO: ensure rand_formula not null or with empty values after symbol
        cat('\t', which(response == var), '/', length(response), ' ', var, sep = '')
        if (omit_NA) {
            dt = na.omit(data[, c(var, const_dt)])
            cat(': omit_NA n:', nrow(dt))
        }
        if (! exists('dt')) {dt <- data[, c(var, const_dt)]}

        model_void = list(formula = as.formula(paste(var, '~ ', rand_formula)), REML = FALSE)
        model_fixed = c()
        for (i in fixed) {
            n = paste('m_', i, sep = '')
            m = list(formula = as.formula(paste(var, '~ ', i, ' + ', rand_formula)), REML = FALSE)
            assign(n, m)
            model_fixed = c(model_fixed, n)
        }
        model_all = list(formula = as.formula(paste(var, fixed_formula, ' + ', rand_formula)),
        REML = FALSE)
        if (fixed_interaction) {
            model_inter = list(formula = as.formula(paste(var, inter_formula, ' + ', rand_formula)),
            REML = FALSE)
        }


        out[[var]] <- list()
        out[[var]][['void']] = do.call('mod.form', model_void)
        out[[var]][['all']] = do.call('mod.form', model_all)
        for (m in model_fixed) {
            out[[var]][[m]] = do.call('mod.form', get(m))
        }
        if (fixed_interaction) {
            out[[var]][['inter']] = do.call('mod.form', model_inter)
        }
        cat('\t\tdone\n')
    }
    class(out) <- append(class(out), "mresp")

    if (check_models) {
        out <- mod.check(out, omit_NA = omit_NA)
    }
    if (choose_models) {
        out <- mod.choose(out)
    }

    # #debug
    # cat('\n\n[debug] final object names:',names(out),'\n\n')
    # cat('[debug] ChosenModelResponse str:')
    # print(str(out[[2]]))
    # cat('\n variable names: ', names(out[[3]]),'\n\n')
    # cat('\n last check_out object: \n')
    # print(out[[length(names(out))]][['check_out']])

    return(out)
}
