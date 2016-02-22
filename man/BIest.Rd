\name{BIest}
\alias{BIest}
\alias{BIest}
\title{
Estimation of the parameters of a binomial distribution
}
\description{
This function calculates the maximum likelihood estimation of the probability parameter in a binomial distribution. It also returns the standard errors of the estimator, and the upper and lower bounds of the interval of confidence. There is the option to include a dispersion parameter in the model, which is estimated by the method of moments.
}
\usage{
BIest(y,n,disp=FALSE)
}
\arguments{
\item{y}{ the variable under the binomial distribution assumption we want to model.}
\item{n}{  the maximum score of the binomial trials.}
\item{disp}{ if TRUE the dispersion parameter will be added to the model. By default it uses the FALSE option.}
}

\value{
\item{p}{ the maximum likelihood estimation of the probability parameter.}
\item{se}{ the standard errors of the mle.}
\item{low.ic}{ the lower bound of the confidence interval of the mle.}
\item{up.ic}{ the upper bound of the confidence interval of the mle.}
\item{phi}{ if the 'disp' option is TRUE, it returns the estimated value of the dispersion parameter. This estimation is done by method of moments, and affects estimation of the standard error of the mle.}
}

\details{
The estimation of the probability parameter is calculated by maximum likelihood considering the binomial distribution as a general exponential family distribution. The log-likelihood is derived and equated to zero, the solution maximizes the log-likelihood. It also returns the standard error of the estimator, which is obtained by the Fisher information formula, i.e., the second derivate of the log-likelihood inserting the mle.

On the other hand, once the mle of the probability parameter has been found out, the dispersion parameter is estimated by the method of moments by the following formula,
\deqn{Var[Y]=\phi np(1-p)}
We replace the estimated value of the probability parameter and the calculated variance of the variable in the formula giving the estimated value of the dispersion parameter. 
}
\author{
Josu Najera Zuloaga

Dae-Jin Lee
}
\references{
Pawitan Y. (2001): In All Likelihood: Statistical Modelling and Inference Using Likelihood, \emph{Oxford University Press}}

\examples{
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
}
