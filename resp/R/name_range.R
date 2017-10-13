# Define function to categorize significance
name_range <- function(x, range) {
    UseMethod('name_range',x)

    # if(is.data.frame(x)) {
    #     return(name_range.data.frame(x,range))
    # } else if(is.numeric(x)) {
    #     return(name_range.numeric(x,range))
    # } else {
    #     stop('Object type not supported for name_range')
    # }
}

name_range.default <- function(x, range) {
    # Chose label based on range as [Low]|(Mid]_n|(High]
    labels=c('Low','Mid','High')
    # if(is.integer()) = sort(range)

    if(as.numeric(x)<=as.numeric(range[1L])) {
        return(labels[1L])
    } else if(as.numeric(x)>as.numeric(range[length(range)])) {
        return(labels[3L])
    } else {
        if(length(range)==2L) {
            return(labels[2L])
        } else {
            mid <- range[-c(1L,length(range))]
            for (i in mid) {
                if(as.numeric(x)<=as.numeric(i)) {
                    return(paste(labels[2L],'_',mid[which(mid=i)]))
                }
            }
        }
    }
}

name_range.numeric <- function(x, range) {
    # Split elements
    a = x
    for (i in 1:length(x)) {
        a[i] <- name_range.default(x=a[i], range=range)
    }
    return(a)
}


name_range.data.frame <- function(x, range) {
    as.data.frame(lapply(x, name_range.numeric, range=range), row.names=row.names(x))
}