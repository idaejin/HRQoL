BBest <- function(y,n){
  
  if (n==as.integer(n)){
  } else {
    stop("The number of trials n must be integer")
  }
  
  if (length(n)>1){
    stop("The number of trials n must be the same for all the observations")
  }
  
  if (n<=0){
    stop("The number of trials n must be positive")
  }
  
  if (class(y)=="factor"){
    stop("The response variable y must be numeric")
  }
  
  if (min(y==as.integer(y))==0){
    stop("The response variable y must be integer")
  } 
  
  if (max(y)>n | min(y) < 0){
    stop("The response variable y must be bounded between 0 and n")
  }

  noObs <- length(y)
  
  m <- mean(y)
  v <- var(y)
  p <- m/n
  phi<- (v-n*p*(1-p))/(n*p*(1-p)*n-v)
  coef <- cbind(n,p,phi)
  
  if (phi < 0){
    stop("There is no overdispersion, the underdispersion cannot be modelled by the beta-binomial distribution; use the binomial estimation instead")
  }
  
  # The variance is estimated through the Fisher Information Matrix:
  Fisher.p <- 0
  Fisher.phi <- 0
  Fisher.pphi <- 0
  for (i in 1:noObs){
    for (k in 0:(y[i]-1)){
      Fisher.p <- Fisher.p-1/(p+k*phi)^2
      Fisher.phi <- Fisher.phi-(k^2)/(p+k*phi)^2
      Fisher.pphi <- Fisher.pphi-k/(p+k*p)^2
    }
    for (t in 0:(n-y[i]-1)){
      Fisher.p <- Fisher.p-1/(1-p+t*phi)^2
      Fisher.phi <- Fisher.phi-(t^2)/(1-p+t*phi)^2
      Fisher.pphi <- Fisher.pphi+t/(1-p+t*phi)
    }
    for (l in 0:(n-1)){
      Fisher.phi <- Fisher.phi+(l^2)/(1+l*phi)^2
    }
  }
  var.p <- -1/Fisher.p
  var.phi <- -1/Fisher.phi
  cov.pphi <- -1/Fisher.pphi  
  vcov <- matrix(c(var.p,cov.pphi,cov.pphi,var.phi),nrow=2,ncol=2)
  
  out <- list(coef=coef, vcov=vcov)
  
  class(out) <- "BBest"
  
  out$call <- match.call()
  
  out
}


print.BBest <- function(x,...){
  cat("Number of trial in the beta-binomial distribution:",x$coef[1],"\n")
  cat("The probability parameter of the beta-binomial distribution:",x$coef[2],"\n")
  cat("The dispersion parameter of the beta-binomial distribution:",x$coef[3],"\n")
}


summary.BBest <- function(object,...){
  p.se <- sqrt(diag(object$vcov)[1])
  p.coef <- cbind(object$coef[2],p.se)
  colnames(p.coef) <- c("Estimate","StdErr")
  
  phi.se <- sqrt(diag(object$vcov)[2])
  phi.coef <- cbind(object$coef[3],phi.se)
  colnames(phi.coef) <- c("Estimate","StdErr")
  
  
  res <- list(call=object$call,coefficients=cbind(p=object$coef[2],phi=object$coef[3]),p.coefficients=p.coef,
              phi.coefficients=phi.coef,n=object$coef[1])
  
  class(res) <- "summary.BBest"
  res
}


print.summary.BBest <- function(x,...){
  cat("Call:\t")
  print(x$call)
  cat("\n")
  cat("Probability parameter estimation:\n")
  print(x$p.coefficients)
  cat("\n")
  cat("Dispersion parameter estimation:\n")
  print(x$phi.coefficients)
  cat("\nNumber of trials in the beta-binomial model:",x$n )
  
}
