\name{log.dataset}
\alias{log.dataset}
\title{
log-transform data.frame
}
\description{
given a whole dataset in data.frame format, log-transform the values avoiding NAs and zeros,
so no -Inf or NaN is created
}
\usage{
log.dataset(data, columns = 1:dim(data)[2])
}
\arguments{
  \item{data}{
data.frame to be transformed
}
  \item{columns}{
columns to be transformed, DEFAULT: whole data.frame
}
}

\value{
Returns the same dataframe with log-transformed values
}
