\name{BIMreg}
\alias{BIMreg}
\title{
Generalized Linear Mixed Models (GLMMs) for binomial data}
\description{
This model fits a logistic regression with Gaussian random effects. The random effects are assumed to be distributed as a normal distribution with zero mean.}
\usage{
BIMreg(formula,n,random,data)
}

\arguments{
\item{formula}{ an object of class \code{"formula"} (or one that can be coerced to that class): a symbolic description of the model to be fitted.}
\item{n}{ the maximum score of the binomial trials.}
\item{random}{ a vector containing the names of the random components in the data frame, the names must be between quotes.}
\item{data}{ an optional data frame, list or environment (or object coercible by \code{as.data.frame} to a data frame) containing the variables in the model. If not found in data, the variables are taken from environment(formula).}
}

\details{
The model that is performed by this function is a especial case of generalized linear mixed models (GLMMs), in which conditioned on some random components the response variable has a binomial distribution. As in the binomial (logistic) regression a logit link function is applied to the probability parameter of the conditioned distribution, allowing the inclusion of random components in the linear predictor,

\deqn{logit(p)=X\beta+Zu,}
where \eqn{p} is the probability parameter, \eqn{X} a full rank matrix composed by the covariables, \eqn{\beta} the regression coefficients, \eqn{Z} the design matrix for the random effects ang \eqn{u} are the random components. These random effects are independent and has a normal distribution with the same variance and mean 0.

The model estimates the fixed effects and the variance of the random effects, and predicts those random effects for each observation.

The estimation of the parameters is done by likelihood approximation, via iteartive weighted least squares method. The estimation is performed in two steps:  (i) fixed and random parameters are calculated for some given random component variances (ii) variances of the random effects are calculated for some given regression and random coefficients. The estimation approach iterates between (i) and (ii) until convergence is obtained.
}

\value{
  \code{BImmreg} returns an object of class "\code{BIMreg}".
  
  The function \code{summary} (i.e., \code{summary.BIMreg}) can be used to obtain or print a summary of the results, \code{coef} can be used to print the regression coefficients of the model, \code{fitted} (i.e., \code{fitted.BIMreg}) to print the fitted values and \code{residuals} (i.e., \code{residuals.BIMreg}) to get the residuals.

  \item{coefficients}{ the estimated value of the fixed coefficients in the regression.}
  \item{coef.vcov}{ the variance and covariance matrix of the estimated fixed coefficients in the regression.}
  \item{random.coef}{ the predicted random effects of the regression. when more than one random effect is included the prediction of all the random coefficients is given, following the order defined in parameter \code{random}.}
  \item{random.var}{ the variance of each random effect.}
  \item{deviance}{ the deviance of the model based on the approximated log-likelihood.}
  \item{fitted.values}{ the fitted mean values of the probability parameter of the conditioned binomial distribution, obtained by transforming the linear predictor by the inverse of the link function.}
  \item{residuals}{ the residuals of the model.}
  \item{random.se}{ the standard error of each random effect, the esquare of the variance of the random effects.}
  \item{working}{ the final working vector of the iterative weighted least square method.}
  \item{iter}{ the number of iterations in the estimation method.}
  \item{nObs}{ the number of observations in the data.}
  \item{nrand}{ the number of random components in the model.}
  \item{cluster}{ the number of levels in each random component.}
  \item{y}{ the dependent variable in the model.}
  \item{X}{ the model matrix of the fixed effects.}
  \item{Z}{ the model matrix of the random effects.}
  \item{D}{ the variance and covariance matrix of the random effects.}
  \item{n}{ the maximum score of the conditioned binomial trials.}
  \item{W}{ the final weights matrix in the iterative weighted least square method.}
  \item{call}{ the matched call.}
  \item{formula}{ the formula supplied.}
}

\author{
Josu Najera

Dae-Jin Lee
}

\references{
Breslow N. E. & Calyton D. G. (1993): Approximate Inference in Generalized Linear Mixed Models, \emph{Journal of the American Statistical Association}, 88, 9-25

McCulloch C. E. & Searle S. R. (2001): Generalized, Linear, and Mixed Models, \emph{Jhon Wiley & Sons}

Pawitan Y. (2001): In All Likelihood: Statistical Modelling and Inference Using Likelihood, \emph{Oxford University Press}

}

\keyword{matrixcalc, rootSolve }

\examples{
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

}