formula_from_vec <- function(x, start='~ ', mid=' + ', end='', as_formula=FALSE) {
    # Formula components creator
    if (length(x) > 0) {a = paste(start, x[1], sep = '')}
    else {stop('[!] Empty vector')}
    if (length(x) > 1) {
        for (i in 2 : length(x)) {
            a <- paste(a, x[i], sep = mid)
        }
    }
    a = paste(a, end, sep = '')
    if (as_formula) {
        return(as.formula(a))
    } else {
        return(a)
    }
}
