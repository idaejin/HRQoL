dBI <- function(n,p,phi=1){
  
  if (n==as.integer(n)){
  } else {
    stop("n must be integer")
  }
  
  if (p<0 | p >1){
    stop("The probability must be bounded between 0 and 1")
  }
  
  if (phi < 0){
    stop("The dispersion parameter phi must be positive")
  }

if (phi==1){
  s <- dbinom(0:n,n,p)
} else{
  cont0 <- 0
  logout0 <- -(1/2)*log(2*pi*phi*n*p*(1-p))-(1/(2*phi))*((n-cont0)*log((1-cont0/n)/(1-p)))
  out0 <- exp(logout0)
  
  if (n>1){
    cont <- 1:(n-1)
    logout <- -(1/2)*log(2*pi*phi*n*p*(1-p))-(1/(2*phi))*(cont*log(cont/(n*p))+(n-cont)*log((1-cont/n)/(1-p)))
    out <- exp(logout)
    out1 <- c(out0,out)
  }
  
  contn <- n
  logoutn <- -(1/2)*log(2*pi*phi*n*p*(1-p))-(1/(2*phi))*(contn*log(contn/(n*p)))
  outn <- exp(logoutn)
  
  s <-c(out1,outn)/sum(out1,outn)
}

s
}
