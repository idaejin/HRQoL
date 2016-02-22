BIest <- function(y,n,disp=FALSE){
  
  if (class(y)=="factor"){
    stop("The binomial data y must be numeric")
  }
  
  if (min(y==as.integer(y))==0){
    stop("The binomial data y must be integer")
  } 
  
  if (max(y)>n | min(y) < 0){
    stop("The binomial data y must be bounded between 0 and n")
  }
  if (n==as.integer(n)){
  } else {
    stop("n must be integer")
  }
  
  m <- length(y)
  # We compute the MLE using the likelihood function
  p <- sum(y)/(n*m)
  
  # Ones we have computed the mle, we replace it in the Fisher information formula
  I <- m*n/(p*(1-p))
  
  
  if (disp){ 
    
    if (m==1){
      stop("Warning: Cannot calculate overdipersion with one observation")
    }
    a <- y==y[1]
    if (sum(a)==m){
      stop("Warning: Cannot calculate overdispersion if all the observations are equal")
    }
    # Estimation of the dispersion parameter
    # Using the profile likelihood, the bias corrected estimation
    # phi <- BIdisp(y,n,mle)
    # Using the method of moments
    phi <- sum((y-n*p)^2/(n*p*(1-p)))/(m-1)
    
    # Standard deviations
    se <- sqrt(phi/I)
    ic <- c(p-1.96*se,p+1.96*se)
    out <- cbind(p,se,ic[1],ic[2],phi)
    colnames(out) <- c("p","se","low.ic","up.ic","phi")
  }
  else{ 
  se <- 1/sqrt(I)
  ic <- c(p-1.96*se,p+1.96*se)
  out <- cbind(p,se,ic[1],ic[2])
  colnames(out) <- c("p","se", "low.ic","up.ic")
  }
  
  out
}