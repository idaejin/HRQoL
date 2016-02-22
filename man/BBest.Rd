\name{BBest}
\alias{BBest}
\alias{BBest}
\title{
Estimation of the beta-binomial distribution parameters
}

\description{
This function performs the estimation of the beta-binomial distribution parameters, \eqn{p} and \eqn{\phi}, by the method of moments. The variance of the estimations is performed using maximum likelihood by the analytically determined beta-binomial density function.
}

\usage{
BBest(y,n)
}

\arguments{
\item{y}{ the outcome variable that is supposed to be distributed as a beta-binomial distribution.}
\item{n}{ the maximum score of the beta-binomial trials.}
}

\value{
The function \code{summary} (i.e., \code{summary.BBest}) can be used to obtain or print a summary of the results.

\item{n}{ the maximum score of the Binomial trials.}
\item{p}{ the estimated probability parameter.}
\item{phi}{ the estimated dispersion parameter.}
\item{vcov}{ the variance and covariance matrix of the parameters of the beta-binomial distribution}
}

\details{
The method of moments assumes that the response variable \eqn{y} follows a beta-binomial distribution and using the moments of first and second order, i.e., the mean and expectation of the beta-binomial distribution gets the estimation of the parameters.

\deqn{E[y]=np}
\deqn{Var[y]=np(1-p)[1+(n-1)\phi/(1+\phi)]}

The dispersion parameter, \eqn{\phi}, of the beta-binomial distribution can only be positive, as the beta-binomial distribution models the overdispersion. So if the estimated \eqn{\phi} parameter by the method of moments is negative, meaning that there exists underdispersion, the beta-binomial model will not be useful and the function will print an error message, suggesting the use of the binomial distribution.

The variance of the estimations is calculated by maximum likelihood by the second derivative of the density function of the beta-binomial distribution, using the Fisher score matrix, and remplacing the estmiated values of the parameters in it.
}

\author{
Josu Najera

Dae-Jin Lee
}
\references{
Arostegui I., Nunez-Anton V. & Quintana J. M. (2006): Analysis of short-form-36 (SF-36): The
beta-binomial distribution approach, \emph{Statistics in Medicine}, 26, 1318-1342
}

\examples{
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
}
