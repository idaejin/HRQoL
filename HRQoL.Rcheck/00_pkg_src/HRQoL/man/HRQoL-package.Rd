\name{HRQoL-package}
\alias{HRQoL-package}
\alias{HRQoL}
\docType{package}

\title{
Health Related Quality of Life Analysis
}

\description{
The main aim of this package is to offer tools for dealing with the Health Related Quality of Life (HRQoL) analysis- This package provides functions based on different regression models for the analysis of outcome from  HRQoL survey data.

The tools for measuring the HRQoL have been developed in questionaries. One of the most commonly used is the Short-Form-36 Health Survey (SF-36), which we have based on in this package. The SF-36 provides information about the HRQoL divided in eight domains, each one related with an aspect of the HRQoL. This domains are bounded between 0 and 100 and can only take some values. The most natural way for the analysis of these domains is considering them distributed as a binomial random variable and recoding them from 0 to n. This package provides a function to perform the ideal recodification of the domains based on \cite{Arostegui et al. (2013)}, \code{SF36rec}. It also give the choice, through \code{HRQoLplot}, to plot the HRQoL of the subjects in an radar/spider plot.

Several instruments for measuring HRQoL have been developed in form of questionnaires, some of them in a generic way and other for specific diseases. The SF-36 has 36 items that are reduced to 8 health dimensions, it was developed and validated by Ware et al. (Ware et al., 1993) and it has been translated and validated into many languages, including Spanish (Alonso et al., 1995). The health domains provided by the SF-36 are bounded between 0 and 100 and can only take some values. The most natural way of analyzing these domains is considering them as binomial distributed and recoding them from 0 to n. This package provides a function, \code{SF36rec}, to perform the ideal recodification of the domains based on Arostegui et al. (2013) proposal. 

The recoded health domains have a binomial distribution, however, the relationship between the mean and variance the binomial distribution assumes is not usually met. Consequently, if we want to analyze the given dimensions as response variables we have to use some modelization approaches that will deal with over-dispersion problems. This packages provides different regression and estimation methodologies based on different approaches to modelize the over-dispersion:

\enumerate{
\item Binomial distribution with dispersion parameter approach, which inserts a parameter, called dispersion parameter, in the variance of the response variable. That way the relationship between the expectation and variance of the response variable is more flexible and it takes into account the over variability that exists in the model.

\item Beta-binomial modeliztion approach, where conditioned on some beta distributed random variables, \eqn{\theta}, the response variable in binomial with probability parameter \eqn{\theta}. This distribution softens the mean and variance restriction, reaching different distribution shapes.

Arostegui et al. (2006) showed that the beta-binomial distribution is adequate to HRQoL data. Moreover, the beta binomial regression (BRR) approach has been proposed in the literature, not only to detect significant predictors of HRQoL when SF-36 is used, but also to analyze and interpret the effect of several explanatory variables on HRQoL (Arostegui et al., 2010). Comparison of the BBR approach with other commonly used modelling approaches in the same context showed that the beta-binomial distribution was a good option to account for over-dispersion in HRQoL analysis and it offered convenient interpretation of the resutls (Arostegui et al., 2013).

\item Generalized Linear Mixed Model (GLMM) approach, which are the extension of GLMs (Generalized Linear Models) to the inclusion of normal distributed random effects. This package is focus on the analysis of HRQoL domains, so the GLMM framework is focus on the binomial distribution of the response variable. This methodology is a very widely used approach to analyze binomial repsonses in different framework, even in HRQoL.
}

Finally, apart form the previously defined modeling approaches, the package provides also some especific plots to draw HRQoL in different populations, where each dimension of the SF-36 is ploted in a scatter plot considering different characteristics of the given population.

}



\references{
Alonso, J., Prieto, L., and Ant\'on, J. M. (1995). La versi\'on espa\~nola del sf-36 health survey (cuestionario de salud sf–36): Un instrumento para la medida de los resultados cl\'inicos. \emph{Medicina cl\'inica}, (104):771–776.

Arostegui I., Nunez-Anton V. & Quintana J. M. (2013): On the recoding of continuous and bounded indexes to a binomial form: an application to quality-of-life scores, \emph{Journal of Applied Statistics}, 40:3, 563-583

Arostegui I., Nunez-Antón V. & Quintana J. M. (2010): Statistical approches to analyse patient-reported outcome as response variables: An application to health-related quality of life, \emph{Statistical Methods in Medical Research}, 21, 189-214

Arostegui I., Nunez-Antón V. & Quintana J. M. (2006): Analysis of short-form-36 (SF-36): The beta-binomial distribution approach, \emph{Statistics in Medicine}, 26, 1318-1342

Breslow N. E. & Clayton D. G. (1993): Approximate Inference in Generalized Linear Mixed Models, \emph{Journal of the American Statistical Association}, 88, 9-25

Fahrmeir L. & Tutz G. (2001): Multivariate Modelling Based on Generalized Linear Mixed Models, \emph{Springer Series in Statistics}

Forcina A. & Franconi L. (1988): Regression analysis with Beta-Binomial distribution, \emph{Revista di Statistica Applicata}, 21

Goldsmith, S. B. (1972). The status of health status indicators. \emph{Health Services Reports}, (87):212–220.

Jiang J. (2007): Linear and Generalized Linear Mixed Models and Their Applications, \emph{Springer Science + Business Media}

McCulloch C. E. & Searle S. R. (2001): Generalized, Linear, and Mixed Models, \emph{Jhon Wiley & Sons}

Pawitan Y. (2001): In All Likelihood: Statistical Modelling and Inference Using Likelihood, \emph{Oxford University Press}

Williams D. A. (1982): Extra-Binomial Variation in Logistic Linear Regression, \emph{Journal of the Royal Statistical Society}, 31, 144-148

Ware, J. E., Snow, K. K., Kosinski, M. A., and Gandek, B. (1993). SF–36 Health Survey, Manual and Interpretation Guides. \emph{MA: The Health Institute}, New England Medical Center.
}

\examples{
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
}
