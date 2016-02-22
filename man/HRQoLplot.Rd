\name{HRQoLplot}
\alias{HRQoLplot}
\alias{HRQoLplot}
\title{
Spider plot of the dimensions of the Short Form-36 Health Survey
}
\description{
This function creates a spider plot with all the 8 dimensions of the SF-36 questionnaire indicating the HRQoL score of each observation.
}
\usage{
HRQoLplot(data,legend)
}
\arguments{
\item{data}{ a data frame with each column relative to observations of each dimension. The columns of the data frame must be introduced in the following order:
\enumerate{
\item  column -> Role Physical
\item  column -> Role Emotional
\item  column -> Physical Functioning
\item  column -> Vitality
\item  column -> Social Functioning
\item  column -> Body Pain
\item  column -> General Health
\item  column -> Mental Health
}
}
\item{legend}{ logical parameter, if \code{TRUE} the legend with the name of the rows of the data will appear. \code{FALSE} has been taken by default.}
}

\details{
The \code{HRQoLplot} has a data frame object as input, and an optional \code{legend} logical input. The data frame must have observations of the HRQoL taken by the SF-36 and the columns of the data frame will refer to each domain of the SF-36, hence the data frame must have eight columns. Each axis of the plot refers to an especific HRQoL domain, so the order of the data frame columns must be as it is showed in the arguments section. The HRQoL score of each observation (row) of the data frame is drawn with a line of a different color in the plot.
}

\author{
Josu Najera Zuloaga

Dae-Jin Lee

This function depends on the function \code{radarchart} of the package \code{fmsb} created by Minato Nakazawa.

}

\keyword{ fmsb }
\seealso{
As it is said in the author section, the function depends on the function \code{\link{radarchart}} of the package \code{fmsb}
}
\examples{
set.seed(5)
# We insert the columns in the order that has been determined:
n <- c(4,3,20,20,8,9,20,13)
k=3
p=runif(8,0,1)
phi <- runif(8,1,3)
dat <- data.frame(
  RP=rBB(k,n[1],p[1],phi[1]),
  RE=rBB(k,n[2],p[2],phi[2]),
  PF=rBB(k,n[3],p[3],phi[3]),
  VT=rBB(k,n[4],p[4],phi[4]),
  SF=rBB(k,n[5],p[5],phi[5]),
  BP=rBB(k,n[6],p[6],phi[6]),
  GH=rBB(k,n[7],p[7],phi[7]),
  MH=rBB(k,n[8],p[8],phi[8]))

rownames(dat) <- c("ID1", "ID2", "ID3")
HRQoLplot(dat,TRUE)
}