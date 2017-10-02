\name{mod.resp}
\alias{mod.resp}
\title{
Create response lmer models for a whole data.frame
}
\description{
Given the fixed and random factors of a data.frame, creates a list of 5 mixed models for each response variable,
 so the best fit can be selected
}
\usage{
mod.resp(data, fixed, random, r_group, exclude,
 omit_NA = TRUE, fixed_interaction = TRUE, check_models = TRUE)
}
\arguments{
  \item{data}{
\code{data.frame} to be analyzed
}
  \item{fixed}{
\code{vector} of column names to be used as fixed factors
}
  \item{random}{
\code{vector} of column names to be used as random factors
}
  \item{r_group}{
\code{vector} of column names to be used as random grouping factors
}
  \item{exclude}{
\code{vector} of column names to be excluded from the response analysis
}
  \item{omit_NA}{
\code{logical}, DEFAULT \code{TRUE}, avoid using NAs from \code{data}
}
  \item{fixed_interaction}{
\code{logical}, DEFAULT \code{TRUE}, check interaction from \code{fixed} factors
}
  \item{check_models}{
\code{logical}, DEFAULT \code{TRUE}, create \code{check_out} table for each response variable
}
}

\value{
Returns \code{mresp} object with a list of response variables sorted alphabetically,
 with 5 models each and a comparision between them (\code{checkout})
%%  If it is a LIST, use
    \item{resp_1}{First response variable}
    \item{resp_{...}}{Other response variables}
    \item{resp_n}{Last response variable}
%% ...
}
\references{
\code{lmer}, \code{stats}
}
\examples{

x <- data.frame(
    Treatment=rep(c('T1','T2','T3'),5),
    Specie=rep(c('S1','S2','S3'),5),
    Group=rep(c('A','B','C'),5),
    Rand=runif(n = 15),
    Heigth=c(runif(n=15)*10),
    Diameter=c(seq(from=2,to=8,length.out=15)*runif(n=1)),
    Number_leaves=c(seq(from=20,to=800,length.out=15)),
    other=runif(n=15)
)
mod.resp(data = x, fixed = c('Treatment','Specie'), random='Rand', r_group = c('Group'),exclude='other')

}
