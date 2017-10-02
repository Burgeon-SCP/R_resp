pkgname <- "resp"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('resp')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("mod.resp")
### * mod.resp

flush(stderr()); flush(stdout())

### Name: mod.resp
### Title: Create response lmer models for a whole data.frame
### Aliases: mod.resp

### ** Examples


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




cleanEx()
nameEx("resp-package")
### * resp-package

flush(stderr()); flush(stdout())

### Name: resp-package
### Title: Creates response models for whole datasets
### Aliases: resp-package resp
### Keywords: package

### ** Examples

~~ simple examples of the most important functions ~~



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
