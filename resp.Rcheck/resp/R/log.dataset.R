log.dataset <-
function(data,columns=1:dim(data)[2]) {
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
