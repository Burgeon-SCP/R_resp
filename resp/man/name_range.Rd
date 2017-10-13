\name{name_range}
\alias{name_range}
\title{
Numeric data categorization between user-given ranges
}
\description{
Returns 'High', ['Mid[_n]'] or 'Low' based on range values for group limits
}
\usage{
name_range(x, range)
}
\arguments{
  \item{x}{
\code{numeric} set to be transformed, must be \code{vector} or \code{data.frame}
}
  \item{range}{
limits of the groups to be created
}

}
\value{
Returns \code{vector} or \code{data.frame} of transformed elements.
}
