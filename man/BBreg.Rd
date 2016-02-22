\name{BBreg}
\alias{BBreg}
\alias{BBreg}
\title{
  Fitting Beta-binomial regression models
}
\description{
  \code{BBreg} is used to fit beta-binomial regression models, where the estimation is performed using maximum likelihood.
}
\usage{
  BBreg(formula,n,data)
}
\arguments{
  \item{formula}{ an object of class \code{"formula"} (or one that can be coerced to that class): a symbolic description of the model to be fitted.}
  \item{n}{ the maximum score of the binomial trials.}
  \item{data}{ an optional data frame, list or environment (or object coercible by \code{as.data.frame} to a data frame) containing the variables in the model. If not found in data, the variables are taken from environment(formula).}
}
\value{
  \code{BBreg} returns an object of class "\code{BBreg}".
  
  The function \code{summary} (i.e., \code{summary.BBreg}) can be used to obtain or print a summary of the results.
  
  The generic accessor functions \code{coefficients}, \code{fitted.values} and \code{residuals} can be used to extract various useful features of the value returned by \code{BBreg}.
  
  \item{coefficients}{ the estimated value of the regression coefficients.}
  \item{vcov}{ the variance and covariance matrix of the estimated coefficients of the regression.}
  \item{phi.coefficient}{ the logarithm of the estimated value of the dispersion parameter of a beta-binomial distribution model.}
  \item{phi.var}{ the variance of the logarithm of the estimated dispersion parameter.}
  \item{fitted.values}{ the fitted mean values, obtained by transforming the linear predictors by the inverse of the link function.}
  \item{residuals}{ the working residuals, that is the residuals in the final iteration of the IWLS fit.}
  \item{df}{ degrees of freedom of the model.}
  \item{deviance}{ the deviance of the model, i.e., minus twice the maximized log-likelihood.}
  \item{iter}{ number of iterations in the iterative weighted least squares method.}
  \item{X}{ the model matrix.}
  \item{y}{ the dependent variable in the model.}
  \item{n}{ the maximum score of the Binomial trials.}
  \item{noObs}{ number of observations in the data frame.}
  \item{call}{ the matched call.}
  \item{formula}{ the formula supplied.}
}
\details{
The probability parameter of the beta-binomial distribution \eqn{p} is connected to a vector of regression parameters by means of a logit link function model. The relationship between the covariables \eqn{X_1,\ldots,X_t} and the probablity parameter of each observation \eqn{p_i}, \eqn{i=1,\ldots,m} where \eqn{m} is the number of observations, is defined as
\deqn{p_i=exp(x_i'\beta)/(1+exp(x_i'\beta)),}
where \eqn{\beta} is a \eqn{(t+1)\times 1} vector of regression parameter, and \eqn{x_i} is the \eqn{i}th row of a full design matrix \eqn{X} composed by the covariables.

The marginal density of the beta-binomial distribution can be explicity calculated, so if we replace the previous equation on the probability parameter of the density function we can perform estimations by maximum likelihood in the distribution parameters. Forcina and Franconi (1988) presented an iterative weighted least square method to perfom estimation on the both, regression parameters and, the dispersion parameter of the beta-binomial distribution.

If the estimation of the dispersion parameter \eqn{\phi} in the iterative weigthing least squares method is lower than \eqn{0.05/[(n-1)-0.05)]}, where \eqn{n} is the number of trials, the function will use the usual logistic regression based on a binomial distribution, as a lower estimation of the dispersion parameter than the given value means that there is no dispersion problem in the model.
}

\author{
  Josu Najera
  
  Dae-Jin Lee
}
\references{
Forcina A. & Franconi L. (1988): Regression analysis with Beta-Binomial distribution, \emph{Revista di Statistica Applicata}, 21.
}

\examples{
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
  
  # We simulate a beta-binomial random variable for those parameters.
  y <- rBB(k,n,p,phi)
  
  # We perform the beta-binomial regression
  BBreg(y~x,n)
}
