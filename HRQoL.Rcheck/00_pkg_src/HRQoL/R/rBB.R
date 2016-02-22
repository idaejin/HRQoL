rBB <- function(k,n,p,phi){
  
  if ((n%%1==0)=="FALSE"){
    stop("the number of trials n must be integer")
  }
  
  if (k==as.integer(k)){
  } else {
    stop("The number of simulations must be integer")
  }
  
  if (k<=0){
    stop("The number of simulations must be positive")
  }
  
  if (min(p)<0 | max(p)>1){
    stop("The probability parameter p must be bounded between 0 and 1")
  }
  
  if (phi<0){
    stop("The dispersion parameter phi must be positive")
  }
  
  if (phi==0){
    stop("The dipersion parameter phi cannot be zero, use the binomial distribution instead")
  }
  
  
  alpha <-p/phi
  beta <- (1-p)/phi
  
  p. <- rbeta(k,alpha,beta)
  
  out <- rbinom(k,n,p.)
  return(out)
}