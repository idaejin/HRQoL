IWLS <- function(y,X,n){
  
  # Initial values
  beta <- c(1,rep(0,dim(X)[2]-1))
  oldbeta <- rep(100,dim(X)[2])
  iter <- 0
  while (max(abs(beta-oldbeta))>0.001){
    oldbeta <- beta
    p <- 1/(1+exp(-(X%*%beta)))
    # Working vector
    Y <- X%*%beta+(y-n*p)/(n*p*(1-p))
    # Weights
    W <- n*p*(1-p)
    
    WX <- sweep(X,MARGIN=1,sqrt(W),'*')
    U <- solve(crossprod(WX))
    
    beta <-U%*%t(X)%*%as.vector(W*Y)
    iter <- iter+1
    if (iter >20){
      print("The iterative least squares method has not converged")
      return()
    }
  }
  
  # Once we reach convergence we have to compute sigma using the estimate to calculate the
  # variances of betas.
  p <- 1/(1+exp(-(X%*%beta)))
  W <- (n*p*(1-p))
  WX <- sweep(X,MARGIN=1,sqrt(W),'*')
  U <- solve(crossprod(WX))

  return(list(beta=beta,vcov=U,iter=iter))
}