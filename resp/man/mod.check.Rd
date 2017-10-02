\name{mod.check}
\alias{mod.check}
\title{
check models to recover performance information
}
\description{
After a \code{mresp} object is created, check for fixed factors significance and unvalidated results,
 mod.check can also be used to compare the performance of the created mixed models
}
\usage{
mod.check(models,omit_NA=TRUE)
}
\arguments{
  \item{models}{
\code{mresp} object to be analyzed
}
  \item{omit_NA}{
\code{logical}, compare data without NA values, DEFAULT: TRUE
}
}

\value{
Returns the given \code{mresp} object with new \code{check_out} element inside each response variable
}
