\name{BB}
\alias{dBB}
\alias{rBB}
\title{
The Beta-Binomial Distribution
}
\description{
Density and random generation for the beta-binomial distribution.
}
\usage{
dBB(y,n,p,phi)
rBB(k,n,p,phi)}

\arguments{
\item{n}{ the maximum score of the beta-binomial trials.}
\item{k}{ the number of simulations.}
\item{y}{ the number of successes in n beta-binomial trials.}
\item{p}{ the probabilily parameter of the beta-binomial distribution.}
\item{phi}{ the dispersion parameter of the beta-binomial distribution.}
}

\value{
\code{dBB} gives the density of a beta-binomial distribution with the defined \code{n}, \code{p} and \code{phi} parameters.

\code{rBB} generates \code{k} random observations based on a beta-binomial distribution with the defined \code{n}, \code{p} and \code{phi} parameters.
}

\details{
The beta-binomial distribution is defined as a mixture between a binomial distribution and a beta distribution. It assumes that conditioned on some random components \eqn{\theta}, with beta distribution with parameters \eqn{p/\phi} and \eqn{p/(1-\phi)}, the response variable \eqn{y} follows a binomial distribution with probability parameter \eqn{\theta},
\deqn{y|\theta \sim Bin(n,\theta),  \theta \sim Beta(p/\phi,(1-p)/\phi).}
The expectation, variance and density function of this distribution can be explicitly calculated:
\deqn{E[y]=np,}
\deqn{Var[y]=np(1-p)[1+(n-1)\phi/(1+\phi)].}
Consequently, \eqn{p} is called the probability parameter and \eqn{\phi} is called the dispersion parameter of the beta-binomial distribution. Hence, the response variable \eqn{y} follows a beta-binomial distribution of parameters \eqn{n}, \eqn{p} and \eqn{\phi},
\deqn{y \sim BB(n,p,\phi).}
}

\author{
Josu Najera

Dae-Jin Lee
}

\references{
Arostegui I., Nunez-Anton V. & Quintana J. M. (2006): Analysis of short-form-36 (SF-36): The
beta-binomial distribution approach, \emph{Statistics in Medicine}, 26, 1318-1342
}

\keyword{ stats }
\seealso{
The \code{rbeta} and \code{rbinom} functions of package \code{\link[<stats>:<stats>-stats]{<stats>}}
}
\examples{
set.seed(12)
# We define
n <- 10     #maximun number of scores in the binomial trials
p <- 0.4    #probability parameter of the beta-binomial distribution
phi <- 1.8  #dispersion parameter of the beta-binomial distribution

# We perform k beta-binomial simulations for those parameters.
k <- 100
bb <- rBB(k,n,p,phi)

# We are going to plot the histogram of the created variable,
#and using dBB() function we are going to fit the distribution:
hist(bb,col="grey",breaks=seq(-0.5,n+0.5,1),probability=TRUE,main="Histogram",xlab="beta-binomial random variable")
lines(c(0:n),dBB(0:n,n,p,phi),col="red",lwd=4)

}
