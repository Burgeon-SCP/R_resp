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
