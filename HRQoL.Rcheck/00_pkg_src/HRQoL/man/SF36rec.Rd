\name{SF36rec}
\alias{SF36rec}
\alias{SF36rec}
\title{
Short Form-36 Helath Survey (SF-36) Recode
}

\description{
The \code{SF36rec} function recodes the Short Form-36 Health Survey (SF-36) dimensions into binomial data, based on \cite{Arostegui et al. (2013)}.
}

\usage{
SF36rec(x,k)
}

\arguments{
\item{x}{ The dimension that is going to be recoded, it must be numeric and bounded between 0 and 100.}
\item{k}{ An integer from 1 to 8, indicating the SF-36 dimension that \code{x} belongs. These are the dimensions depending on the \code{k} value:

\code{k=1} -> Role Physical

\code{k=2} -> Role Emotional

\code{k=3} -> Physical Functioning

\code{k=4} -> Vitality

\code{k=5} -> Social Functioning

\code{k=6} -> Body Pain

\code{k=7} -> General Health

\code{k=8} -> Mental Health
}
}

\value{
 The dimension recoded to binomial data.
}

\details{
Each observation of the HRQoL measured by the SF-36 is divided in eight domains, and each domain refers to an especific aspect of the HRQoL: \emph{Role Physical, Role Emotional, Physical Functioning, Vitality, Social Functioning, Body Pain, General Health and Mental Health}. This domains are bounded between 0 and 100, and each domain can only take some values. The usual way of analyzing the domains is by the linear regression, but it has been proved that it is not correct \cite{Arostegui et al. (2006)}. The most natural way of analyzing the HRQoL data, and, thus, the eight domains, is by considering them binomially distributed \cite{Arostegui et al. (2006)}. The recodification to the most suitable form is evaluated in \cite{Arostegui et al. (2013)}. This function recodes the domains based on the method that is proposed in that article.

The form to use this function, is by inserting one by one each domain, and by defining with the input \code{k} which domain are we inserting, get the recodification to the binomial form of that domain.

}

\author{
Josu Najera Zuloaga

Dae-Jin Lee
}
\references{
Arostegui I., Nunez-Anton V. & Quintana J. M. (2013): On the recoding of continuous and bounded indexes to a binomial form: an application to quality-of-life scores, \emph{Journal of Applied Statistics}, 40:3, 563-583

Arostegui I., Nunez-Anton V. & Quintana J. M. (2006): Analysis of short-form-36 (SF-36): The beta-binomial distribution approach, \emph{Statistics in Medicine}, 26, 1318-1342}


\examples{
set.seed(2)
#We simulate a variable bounded between 0 and 100, and that only can take some values.
BodyPain <- rnorm(1000,50,15)
k <- 6  #Because the domain we are inserting is the Body Pain
BodyPain.rec <- SF36rec(BodyPain,k)
}
