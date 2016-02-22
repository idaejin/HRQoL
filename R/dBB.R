dBB <- function(y, n, p, phi){
  
  if (sum(y%%1==0)==length(y)){
  } else {
  stop("The number of success y must be integer")
  }
  
  if (max(y)>n){
    stop("The number of success y cannot be bigger than the number of trials")
  }
  
  if (min(y)<0){
    stop("The number of success y must be positive")
  }
  
  if (n<=0){
    stop("The number of trials n must be positive")
  }
  
  if ((n%%1==0)=="FALSE"){
    stop("The number of trials n must be an integer")
  }

  if (length(p)>1){
    stop("The probability parameter p cannot be a vector")
  }
  
  if (p<0 | p>1){
    stop("The probability parameter p must be bounded between 0 and 1")
  }
  
  if (phi<0){
    stop("The dispersion parameter phi must be positive")
  }
  
  if (phi==0){
    stop("The dipersion parameter phi cannot be zero, use the binomial distribution instead")
  }
  

  tt <- y
  tt1 <- gamma(n + 1)/(gamma(tt + 1) * gamma(
    n - tt + 1))
  tt2 <- gamma((p/phi) + tt)/gamma(p/phi)
  tt3 <- gamma(((1 - p)/phi) + n - tt)/
    gamma((1 - p)/phi)
  tt4 <- gamma(1/phi)/gamma((1/phi) + n)
  tt1 * tt2 * tt3*tt4
}