\name{BIreg}
\alias{BIreg}
\alias{BIreg}
\title{
  Fit a logistic regression model
}
\description{
\code{BIreg} is used to fit a logistic regression model, i.e., the probability parameter of a binomial distribution with some given covariables is related by a logit link function. There is the option to include a dispersion parameter which will be estimated by the bias-corrected method of moments.
}
\usage{
  BIreg(formula,n,data,disp=FALSE)
}
\arguments{
\item{formula}{ an object of class \code{"formula"} (or one that can be coerced to that class): a symbolic description of the model to be fitted.}
  \item{n}{ the maximum score of the Binomial trials.}
  \item{data}{ an optional data frame, list or environment (or object coercible by \code{as.data.frame} to a data frame) containing the variables in the model. If not found in data, the variables are taken from environment(formula).}
  \item{disp}{ if \code{TRUE} the dispersion parameter will be estimated. Default mode is set to \code{FALSE}.}
}
\value{
  \code{BIreg} returns an object of class "\code{BIreg}".
  
  The function \code{summary} (i.e., \code{summary.BIreg}) can be used to obtain or print a summary of the results.
  
  The generic accessor functions \code{coefficients}, \code{fitted.values} and \code{residuals} can be used to extract various useful features of the value returned by \code{BIreg}.
  
  \item{coefficients}{ the estimated value of the regression coefficients.}
  \item{vcov}{ the variance and covariance matrix of the estimated coefficients of the regression.}
  \item{phi}{ if the disp option is TRUE, it returns the estimated value of the dispersion parameter; the estimation is realized with the profile likelihood of the dispersion parameter, using the bias-corrected estimation. If the disp option is FALSE, the estimated value will be 1.}
  \item{fitted.values}{ the fitted mean values, obtained by transforming the linear predictors by the inverse of the link function.}
  \item{residuals}{ the working residuals, that is the residuals in the final iteration of the IWLS fit.}
  \item{deviance}{ the deviance of the model, i.e., minus twice the maximized log-likelihood.}
  \item{df}{ the degrees of freedom of the model.}
  \item{null.deviance}{ the deviance for the null model. The null model will include only an intercept.}
  \item{df.null}{ the residual degrees of freedom for the null model.}
  \item{iter}{ number of iterations in the iterative weighted least squares method.}
  \item{X}{ the model matrix.}
  \item{y}{ the dependent variable in the model.}
  \item{n}{ the maximum score of the Binomial trials.}
  \item{noObs}{ number of observations in the data frame.}
  \item{call}{ the matched call.}
  \item{formula}{ the formula supplied.}
}
\details{
The estimation of the regression parameters is done via maximum likelihood, considering the likelihood of the binomial response variable in the general form of a exponential family
\deqn{f(y)=\frac{y log(p/(1-p))+n log(1-p)}{\phi}+c(y,\phi).}
The proabability parametern \eqn{p} has been connected with the given covariables ,$X_1,\ldots,X_t$, by the logit link function as,
\deqn{logit(p)=log(\frac{p}{1-p})=x_i'\beta,}
where \eqn{\beta} are the regression coefficients and \eqn{x_i} is the ith row of a full rank design matrix \eqn{X} composed by the given covariables.
Remplacing the prevous formula in the likelihodd of the response variable \eqn{y}, and by an iterative weigthed least squares algorithm, (\code{\link{IWLS}}), we get the estimations of the regression parameters.

If the \code{disp} option is TRUE, the dispersion parameter is going to be done using the bias-corrected method of moments, that is the same as the method of moments, but divided by the number of observation minus the number of parameters instead of only divided by the number of observations. 
}
\author{
  Josu Najera
  
  Dae-Jin Lee
}
\references{
Pawitan Y. (2001): In All Likelihood: Statistical Modelling and Inference Using Likelihood, \emph{Oxford University Press}

Williams D. A. (1982): Extra-Binomial Variation in Logistic Linear Regression, \emph{Journal of the Royal Statistical Society}, 31, 144-148
}

  \keyword{ HRQoL }
\seealso{
Iterative weighted least squares method function: \code{\link{IWLS}}
}
\examples{
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
}