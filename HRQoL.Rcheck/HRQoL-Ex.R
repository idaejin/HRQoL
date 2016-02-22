pkgname <- "HRQoL"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "HRQoL-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('HRQoL')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("BB")
### * BB

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: BB
### Title: The Beta-Binomial Distribution
### Aliases: dBB rBB
### Keywords: stats

### ** Examples

set.seed(12)
# We define
n <- 10     #maximum number of scores in the binomial trials
p <- 0.4    #probability parameter of the beta-binomial distribution
phi <- 1.8  #dispersion parameter of the beta-binomial distribution

# We perform k beta-binomial simulations for those parameters.
k <- 100
bb <- rBB(k,n,p,phi)

# Show the histogram of the generated variable,
#with dBB() function we fit a beta-binomial distribution:
hist(bb,col="grey",breaks=seq(-0.5,n+0.5,1),probability=TRUE,main="Histogram",xlab="beta-binomial random variable")
lines(c(0:n),dBB(0:n,n,p,phi),col="red",lwd=4)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("BB", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("BBest")
### * BBest

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: BBest
### Title: Estimation of the parameters of beta-binomial distribution
### Aliases: BBest BBest

### ** Examples

# We simulate beta-binomial distributed observations.
n <- 10     # maximum score of the Binomial trials 
k <- 1000   # number of simulated observations
p <- 0.7    # probability parameter of the beta-binomial distribution
phi <- 1.6  # dispersion parameter of the beta-binomial distribution

set.seed(5)
y <- rBB(k,n,p,phi)

# Estimation
est <- BBest(y,n)
print(est)

# Summary of the results
summary(est)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("BBest", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("BBreg")
### * BBreg

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: BBreg
### Title: Fitting Beta-Binomial Regression Models
### Aliases: BBreg BBreg

### ** Examples

  # We will generate a beta-binomial response variable fixing some values of 
  # the regression parameters and simualting a random effect. Then we are going
  # to proof that we reach the same values.
  
  # We generate the outcome variable fromm a simulated covariable.
  set.seed(11)
  k <- 100
  n <- 10
  x <- rnorm(k,5,3)
  
  # We calculate the probability parameter as in the proposed methodology.
  p <- 1/(1+exp(-(2*x-10)))
  phi <- 1.2
  
  # We simualte a beta-binomial variable for those parameters.
  y <- rBB(k,n,p,phi)
  
  # We perform the beta binomial regression, reaching very similar values
  BBreg(y~x,n)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("BBreg", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("BI")
### * BI

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: BI
### Title: The Binomial distribution with Dispersion Parameter
### Aliases: dBI rBI
### Keywords: stats

### ** Examples

k <- 1000
n <- 10
p <- 0.765
phi <- 4.35

#simulating
y <- rBI(k,n,p,phi)
y

#density function
d <- dBI(n,p,phi)
d




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("BI", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("BIMreg")
### * BIMreg

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: BIMreg
### Title: Generalized Linear Mixed Models (GLMMs) for binomial data
### Aliases: BIMreg
### Keywords: matrixcalc, rootSolve

### ** Examples

set.seed(5)
# Creating the dependent variable with a simulated covariable:
nObs <- 500
x <- rnorm(nObs,4,1) 
id1 <- c(kronecker(seq(1,5),rep(1,100)))
id2 <- c(kronecker(seq(1,10),rep(1,50)))


p <- 1/(1+exp(-(5-5*x+kronecker(rnorm(5,0,0.5),rep(1,100))+kronecker(rnorm(10,0,1.2),rep(1,50)))))
y <- rbinom(nObs,10,p)
dat <- data.frame(cbind(y,x,id1,id2))
dat$id1 <- as.factor(dat$id1)
dat$id2 <- as.factor(dat$id2)

#Estimating the mixed model for one random component.
mm1 <- BIMreg(y~x,10,c("id1"),dat)
mm1
summary(mm1)

#Estimating the mixed model for two random components.
mm2 <- BIMreg(y~x,10,c("id1","id2"),dat)
mm2
summary(mm2)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("BIMreg", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("BIest")
### * BIest

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: BIest
### Title: Estimation of the parameters of a binomial distribution
### Aliases: BIest BIest

### ** Examples

set.seed(9)
# We simulated the binomial data with some parameters and then
# we are going to try to reach the same estimations.
n <- 10             # the maximum score of the Binomial trials
k <- 100            # number of simulations
p <- 0.654          # probability parameter
y <- rbinom(k,n,p)  # simulations

# without overdispersion
BIest(y,n) #no overdispersion by default

# with overdispersion
BIest(y,n,TRUE)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("BIest", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("BIreg")
### * BIreg

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: BIreg
### Title: Fit a logistic regression model
### Aliases: BIreg BIreg
### Keywords: HRQoL

### ** Examples

  set.seed(1234)
  # We simulate a covariable and we are going to construct the outcome
  # variable usign the logit link function and the created covariable.
  
  n <- 10                       # maximum score of the Binomial trials
  k <-100                       # number of observations
  covariable <- rnorm(k,2,0.5)  # the covariable
  
  p <- 1/(1+exp(-(-6+4*covariable)))
  outcome <- rbinom(k,n,p)
  
  # without dispersion parameter
  bat <- BIreg(outcome~covariable,n,disp=FALSE)
  summary(bat)
  
  # considering dispersion parameter
  bi <- BIreg(outcome~covariable,n,disp=TRUE)
  summary(bi)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("BIreg", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("HRQoL-package")
### * HRQoL-package

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: HRQoL-package
### Title: Health Related Quality of Life Analysis
### Aliases: HRQoL-package HRQoL

### ** Examples

set.seed(123)
#  Number of observations
k <- 100
#  Maximun number of score in the binomial trials:
n <- 10
#  Probability:
p <- 0.5
#  Dispersion parameter:
phi <- 2
#  We simulate a overdispersed random variable following the beta-binomial distribution:
y <- rBB(k,n,p,phi)

#  We calculate the mle of the parameters using the BIest function,
#   binomial estimation with overdispersion:
est <- BIest(y,n,disp=TRUE)
est
est.p <- est[1]
est.phi <- est[5]

# If we plot it:
hist(y,col="grey",breaks=seq(-0.5,10.5,1),probability = TRUE)
lines(c(0:n),dBI(n,est.p,est.phi),col="red",lwd=4)

# Now we are going to calculate the mle of the parameters using
# the BBest function, beta-binomial:
out <- BBest(y,n)$coef
out
out.p <- out[2]
out.phi <- out[3]
# If we plot it:
hist(y,col="grey",breaks=seq(-0.5,10.5,1),probability = TRUE)
lines(c(0:n),dBB(0:n,n,out.p,out.phi),col="red",lwd=4)

#  Perform a regression:
x <- rnorm(100,2,2)
# Binomial with overdispersion distribution:
BIreg(y~x,10,disp=TRUE)
# Beta-binomial regression:
BBreg(y~x,n)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("HRQoL-package", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("HRQoLplot")
### * HRQoLplot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: HRQoLplot
### Title: Spider plot of the dimensions of the Short Form-36 Health Survey
### Aliases: HRQoLplot HRQoLplot
### Keywords: fmsb

### ** Examples

set.seed(5)
# We insert the columns in the order that has been determined:
n <- c(4,3,20,20,8,9,20,13)
k=3
p=runif(8,0,1)
phi <- runif(8,1,3)
dat <- data.frame(
  RP=rBB(k,n[1],p[1],phi[1]),
  RE=rBB(k,n[2],p[2],phi[2]),
  PF=rBB(k,n[3],p[3],phi[3]),
  VT=rBB(k,n[4],p[4],phi[4]),
  SF=rBB(k,n[5],p[5],phi[5]),
  BP=rBB(k,n[6],p[6],phi[6]),
  GH=rBB(k,n[7],p[7],phi[7]),
  MH=rBB(k,n[8],p[8],phi[8]))

rownames(dat) <- c("ID1", "ID2", "ID3")
HRQoLplot(dat,TRUE)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("HRQoLplot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("IWLS")
### * IWLS

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: IWLS
### Title: Iterative Weighted Least Squares
### Aliases: IWLS IWLS

### ** Examples

#we are going to create a variable and a matrix model
k=1000                    #number of observations
n=10                      #the maximum score of the binomial trials
y <- rbinom(k,n,0.87)     #dependent variable
x1 <- rnorm(k,1,50)       #covariable 1
x2 <- rnorm(k,30,9)       #covariable 2
X <- cbind(1,x1,x2)       #model matrix

IWLS(y,X,n)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("IWLS", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("SF36rec")
### * SF36rec

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: SF36rec
### Title: Short Form-36 Helath Survey (SF-36) Recode
### Aliases: SF36rec SF36rec

### ** Examples

set.seed(2)
#We simulate a variable bounded between 0 and 100, and that only can take some values.
BodyPain <- rnorm(1000,50,15)
k <- 6  #Because the domain we are inserting is the Body Pain
BodyPain.rec <- SF36rec(BodyPain,k)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("SF36rec", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
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
