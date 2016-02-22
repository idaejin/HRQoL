\name{BI}
\alias{dBI}
\alias{rBI}
\title{
The Binomial distribution with overdispersion
}
\description{
Density and random generation for the binomial distribution with optional dispersion parameter calculation.
}
\usage{
dBI(n,p,phi)
rBI(k,n,p,phi)}

\arguments{
\item{k}{ number of simulations.}
\item{n}{ the maximum score of the binomial trials.}
\item{p}{ the probability of scoring a success in each binomial trial.}
\item{phi}{ the dispersion parameter of the binomial distribution. If \code{phi}=1, then the simple binomial model will be performed.}
}

\value{
\code{dBI} gives the density of a binomial distribution for those \code{n}, \code{p} and \code{phi} parameters.

\code{rBI} generates \code{k} random observations based on a binomial distribution with those \code{n}, \code{p} and \code{phi} parameters.}

\details{
The inclusion of a dispersion parameter in the variance equation of the binomial distribution relaxes the relationship that is expected between the mean and variance in binomial models,
\deqn{E[y]=np, \quad Var[y]=\phi np(1-p).}
The density function of this binomial model is calculated considering as an exponential family, where the density function has the following form
\deqn{f(y)=\frac{y log(p/(1-p))+n log(1-p)}{\phi}+c(y,\phi),}
where \eqn{c(y,\phi)} is a function that is approximated by the deviance of the model.
}

\author{
Josu Najera Zuloaga

Dae-Jin Lee
}

\references{
Pawitan Y. (2001): In All Likelihood: Statistical Modelling and Inference Using Likelihood, \emph{Oxford University Press.}
}

\keyword{ stats }
\seealso{
The \code{rbinom} function of package \code{\link[<stats>:<stats>-stats]{<stats>}}. This function performs simulations based on a binomial distribution without dispersion parameter.
}
\examples{
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

}
