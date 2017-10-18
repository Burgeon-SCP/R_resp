#!/usr/bin/R

## Function to create anova table of results for peer reviewed literature.
# Author: Adri'a Masip

peer.table <- function(x) {
    ## Function to create anova table of results for peer reviewed literature, after Tallent-Halsell et al. 2002

    # Check class
    if (inherits(x, 'mresp')) {
        # create a list of aov objects based on 'UserDataFrame' and 'ChosenModelResponse'
        mresp=TRUE
        a <- list()
        for (var in colnames(x[['ChosenModelResponse']])) {
            a[[var]] <- summary(aov())[[1L]]
        }
    } else if (inherits(x, c('aov', 'lm'))) {
        ## access list first element and check
        a <- summary(x)[[1L]]
    } else if (inherits(x, 'summary.aov')) {
        a <- x[[1L]]
    } else {stop('[!] Wrong object')}

    if(!mresp) {a <- list(Response=a)}

    var <- c();name <- c();df <- c();SS <- c()
    MS <- c();Fval <- c();Pperm <- c()

    for (l in names(a)) {
        if (inherits(a, c('summary', 'data.frame'))) {} else {stop('[!] Not an aov data.frame')}
        var <- c(var, c(l, "", "")) # Needs more abstraction
        name <- c(name, row.names(tb)[1 : 3]) # Needs more abstraction
        df <- c(df, tb[1 : 3, 'Df'])
        SS <- c(SS, tb[1 : 3, 'Sum Sq'])
        MS <- c(MS, tb[1 : 3, 'Mean Sq'])
        Fval <- c(Fval, tb[1 : 3, 'F value'])
        Pperm <- c(Pperm, tb[1 : 3, 'Prob(perm)'])
    }

    sig <- factor(levels = c("**", "*", ""))
    for (i in 1 : length(Pperm)) {
        if (Pperm[i] < 0.01) {sig[i] <- "**";next}
        if (Pperm[i] < 0.05) {sig[i] <- "*"} else {sig[i] <- ""}
    }

    out <- data.frame(var, name, df, SS, MS, Fval, Pperm, sig)
    colnames(out) <- c('Response Variable', 'Main Factor', 'df', 'SS', 'MS', 'F', 'P perm', 'sig')


    if (is.null(export) == FALSE) {
        cat('[?] Writting csv file.\n')
        write.csv(out, paste(as.character(export), '.csv', sep = ""))
    }
    et <- Sys.time()
    # cat('[?] Total time spend:',et-st)
    return(out)
}

