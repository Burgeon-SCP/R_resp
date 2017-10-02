\name{formula_from_vec}
\alias{formula_from_vec}
\title{
Formula string creation from \code{vector} elements
}
\description{
Given a vector, create a unique string from the elements.
}
\usage{
formula_from_vec(x, start='~ ', mid=' + ', end='', as_formula=FALSE)
}
\arguments{
  \item{x}{
\code{vector} to be transformed
}
  \item{start}{
initial string, DEFAULT: \code{'~ '}
}
  \item{mid}{
between-elements string, DEFAULT: \code{' + '}
}
  \item{end}{
final string, DEFAULT: \code{''}
}
  \item{as_formula}{
\code{logical}, return string as S formula, DEFAULT: \code{FALSE}
}
}

\value{
Returns single string of concatenated elements
}
