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
  \item{lmer_warnings}{
\code{logical}, DEFAULT \code{FALSE}, display \code{lmer()} construction warnings
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
n <- 500L
tr <- c('T1','T2','T3')
sp <- c('S1','S2','S3')
gr <- c('A','B')
F1 <- c();F2 <- c();F3 <- c()
R1 <- c();R2 <- c();R3 <- c()
for (i in 1:n) {
 F1[i] <- tr[round(i/n*3,0)]
 F2[i] <- sp[round(runif(1L,1L,3L),0)]
 F3[i] <- gr[round(runif(1L,1L,2L),0)]
 R1[i] <- rnorm(1,10,2)+runif(1L)+(which(tr==F1[i])*3)
 R2[i] <- rnorm(1,10,2)+runif(1L)+(which(sp==F2[i])*10)
 R3[i] <- rnorm(1,600,20)+runif(1L)+(which(tr==F1[i])*20)
}
table(F1,F2)

x <- data.frame(
    Treatment=F1,
    Specie=F2,
    Group=F3,
    Rand=runif(n),
    Heigth=R1,
    Diameter=R2,
    Number_leaves=R3,
    other=runif(n)
)
# rm(n,tr,sp,gr,F1,F2,F3)
a <- mod.resp(data = x, fixed = c('Treatment','Specie'), random='Rand',
              r_group = c('Group'),exclude='other',lmer_warnings=TRUE,
              choose_models=FALSE,check_models=FALSE)
print(a)
}
