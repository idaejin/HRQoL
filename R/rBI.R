rBI <- function(k,n,p,phi=1){
  
  if (n==as.integer(n)){
  } else {
    stop("n must be integer")
  }
  
  if (min(p)<0 | max(p) >1){
    stop("The probability must be bounded between 0 and 1")
  }
  
  if (phi < 0){
    stop("The dispersion parameter phi must be positive")
  }
  
  if (phi==1){
    out <- rbinom(k,n,p)
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
    
    p <-c(out1,outn)/sum(out1,outn)
    
    #We divide the [0,1] interval by those points.
    l <- NULL
    for (i in 1:(n+1)){
      l[i] <- sum(p[1:i])
    }
    
    
    out <- NULL
    for (j in 1:k){
      u <- runif(1,0,1)
      for (i in (n+1):1){
        if (u < l[i]){
          o <- i-1 
        }
      }
      out[j] <- o
    
    }
  }
  
  out
}
