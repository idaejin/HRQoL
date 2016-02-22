\name{IWLS}
\alias{IWLS}
\alias{IWLS}
\title{
Iterative Weighted Least Squares
}
\description{
\code{IWLS} gives a method to estimate the coefficients of logistic regressions by maximum likelihood.
}
\usage{
IWLS(y,X,n)
}
\arguments{
\item{y}{ the dependent variable of the model.}
\item{X}{ the model matrix.}
\item{n}{ the maximum score of the binomial trials.}
}
\value{
\item{beta}{ maximum likelihood estimations of the logistic regression coefficients.}
\item{vcov}{ variance-covariance matrix of the estimated coefficients.}
\item{iter}{ the number of iterations of the algorithm.}
}

\details{
The iterative weighted least squares (IWLS) is a general algorithm to find the maximum likelihood estimations (mle) and standard deviations in generalized linear mixed models. There are several ways to derive it, but the one that has been developed in this function is via the Newton-Raphson method. It consists of making a Taylor expansion in the score function, the first derivate of the log-likelihood, around the mle. This especific IWLS, \code{IWLS}, has been developed to find out the mle and the standard errors in logistic regression by the introduction of a dependent variable, a matrix model of the regression and the the maximum score of the binomial trials.
}
\author{
Josu Najera Zuloaga

Dae-Jin Lee
}
\references{
Pawitan Y. (2001): In All Likelihood: Statistical Modelling and Inference Using Likelihood, \emph{Oxford University Press}.
}

\examples{
#we are going to create a variable and a matrix model
k=1000                    #number of observations
n=10                      #the maximum score of the binomial trials
y <- rbinom(k,n,0.87)     #dependent variable
x1 <- rnorm(k,1,50)       #covariable 1
x2 <- rnorm(k,30,9)       #covariable 2
X <- cbind(1,x1,x2)       #model matrix

IWLS(y,X,n)
}
