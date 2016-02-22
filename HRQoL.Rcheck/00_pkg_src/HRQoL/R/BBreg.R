BBreg <- function(formula,n,data=list()){
  
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
  
  
  mf <- model.frame(formula=formula, data=data)
  X <- model.matrix(attr(mf, "terms"), data=mf)
  y <- model.response(mf)
  noObs <- length(y)
  
  if (class(y)=="factor"){
    stop("The response variable y must be numeric")
  }
  
  if (min(y==as.integer(y))==0){
    stop("The response variable y must be integer")
  } 
  
  if (max(y)>n | min(y) < 0){
    stop("The response variable y must be bounded between 0 and n")
  }
  
  # We define the bound of r, as it is bigger than this number the difference in the dispersion that
  # assumes the binomial model is very small.
  bound <- (n-1)/0.05-1
  
  # We calculate which y are 0 or n, we need to calculate the profile likelihood of r.
  t0 <- which(y==0)
  if (length(t0!=0)){
    y0 <- y[-t0]
  }else{
    y0 <- y
  }
  
  tn <- which(y==n)
  if (length(tn!=0)){
    yn <- y[-tn]
  }else{
    yn <- y
  }
  
  # Big loop
  # Initial values
  iter <- 0
  r <- 1
  oldr <- 10000
  beta <- c(1,rep(0,dim(X)[2]-1))
  
  while (abs(r-oldr)>0.1){
    iter <- iter +1
    oldr <- r
    oldbeta <- rep(Inf,dim(X)[2])
    
    # Beta loop
    while (max(abs(beta-oldbeta))>0.001){
      oldbeta <- beta
      p <- 1/(1+exp(-(X%*%beta)))
      
      # Compute S
      s <- p*(1-p)
      
      # Compute u and v
      # Be aware that if y=0 the summatory cannot be computed from 1 to 0.
      u <- NULL
      v <- NULL
      for (j in 1:noObs){
        u1 <- 0
        u2 <- 0
        v1 <- 0
        v2 <- 0
        if (y[j]==0){
          u1 <- 0
          v1 <- 0
        }else{
          for (i in 1:y[j]){
            u1 <- u1+r/(r*p[j]+y[j]-i)
            v1 <- v1+(r^2)/((r*p[j]+y[j]-i)^2)
          }
        }
        if (y[j]==n){
          u2 <- 0
          v2 <- 0
        }else{
          for (i in 1:(n-y[j])){
            u2 <- u2+r/(r-r*p[j]+n-y[j]-i)
            v2 <- v2+(r^2)/((r-r*p[j]+n-y[j]-i)^2)
          }
        }
        u <- c(u,u1-u2)
        v <- c(v,v1+v2)
      }
      
      # Computing y
      sv <- s*v
      Y <- X%*%beta+(1/sv)*u
      
      # Updating beta
      #####    H <- solve(t(X)%*%S%*%V%*%S%*%X)
      #####    beta <- H%*%t(X)%*%S%*%V%*%S%*%Y
      XSV <- sweep(X,MARGIN=1,s*sqrt(v),'*')
      H <- solve(crossprod(XSV))
      beta <- H%*%t(X)%*%as.vector(s*v*s*Y)
    }
    
    # Once we obtain beta, we have to compute p
    p <- 1/(1+exp(-(X%*%beta)))
    
    # Once we reach convergence, we have to update r using the profile likelihood!!!!
    
    # Newton-Rphson to estimate r (using optim)
    
    # We compute p, relevant to each y0 or yn
    if (length(t0!=0)){
      p0 <- p[-t0]
    }else{
      p0 <- p 
    }
    if (length(tn!=0)){
      pn <- p[-tn]
    }else{
      pn <- p
    }
    
    # We define the profile likelihood of r
    Lr <- function(rr){       
      Lr1 <- 0
      Lr2 <- 0
      Lr3 <- 0
      
      for (j in 1:length(y0)){
        for (i in 1:y0[j]){
          Lr1 <- Lr1+log((p0[j]*rr+y0[j]-i))
        }
      }
      for (j in 1:length(yn)){
        for (i in 1:(n-yn[j])){
          Lr2 <- Lr2+log((rr-pn[j]*rr+n-yn[j]-i))
        }
      }
      for (j in 1:noObs){ 
        for (i in 1:n){
          Lr3 <- Lr3+log(rr+n-i)
        }
      }
      out <- -(Lr1+Lr2-Lr3)
      return(out)
    }
    # We find out the mle, r, based on the profile likelihood
    mle <- optim(r,Lr,method="L-BFGS-B",hessian=TRUE,lower=c(1e-4),upper=bound)
    r <- mle$par
  }
  
  if (r==bound){
    print("There is no dispersion problem, the logistic regression has been used instead; BIreg")
    return(BIreg(formula,n,data,disp=FALSE)) 
  }
  
  
  #RELEVANT TO BETAs
  vcov.b <- H
  coef.b <- beta
  if (length(beta)==1){
    rownames(coef.b) <- c("Intercept")
  } else{
    rownames(coef.b) <- c("Intercept",rownames(beta)[2:length(beta)])
  }
  
  #RELEVANT TO r, or phi, but we are going to give the estimation of log(phi) as gamlss does
  phi <- 1/r
  
  Lp <- function(pphi){       
    Lr1 <- 0
    Lr2 <- 0
    Lr3 <- 0
    
    for (j in 1:length(y0)){
      for (i in 1:y0[j]){
        Lr1 <- Lr1+log((p0[j]*(1/exp(pphi))+y0[j]-i))
      }
    }
    for (j in 1:length(yn)){
      for (i in 1:(n-yn[j])){
        Lr2 <- Lr2+log(((1/exp(pphi))-pn[j]*(1/exp(pphi))+n-yn[j]-i))
      }
    }
    for (j in 1:noObs){ 
      for (i in 1:n){
        Lr3 <- Lr3+log((1/exp(pphi))+n-i)
      }
    }
    out <- -(Lr1+Lr2-Lr3)
    return(out)
  }
  log.phi <- log(phi)
  
  bob <- optim(log(phi),Lp,method="L-BFGS-B",hessian=TRUE,lower=c(-1e12),upper=bound)
  phi.var <- solve(bob$hessian)
  
  
  #Fitted value and residuals
  fitted.values <- p
  residuals <- c(y- n*fitted.values)
  
  #DEVIANCE
  #No hay que multiplicar por "-" porque al optimizar la función hemos definido la inversa para 
  #maximizar, pero la parte de la constante si!
  deviance <- as.numeric(2*(-sum(lgamma(n+1)-(lgamma(y+1)+lgamma(n-y+1)))+Lr(r)))
  
  #Degrees of freedom
  df <- noObs-length(beta)-1
  
  out <- list(coefficients=coef.b,vcov=vcov.b,
              phi.coefficient=log.phi,phi.var=phi.var,
              fitted.values=fitted.values,residuals=residuals,
              df=df,deviance=deviance,iter=iter,X=X,y=y,n=n,noObs=noObs)
  
  class(out) <- "BBreg"
  
  out$call <- match.call()
  out$formula <- formula
  
  out
  
}


print.BBreg <- function(x,...){
  cat("Call:\t")
  print(x$call)
  cat("\nBeta coefficients:\n")
  print(t(x$coefficients))
  cat("\nDispersion parameter (log):",x$phi.coefficient,"\n")
  cat("\nDeviance:",x$deviance, " on ", x$df, " degrees of freedom\n")
}


summary.BBreg <- function(object,...){
  beta.se <- sqrt(diag(object$vcov))
  beta.tval <- object$coefficients/beta.se
  beta.TAB <- cbind(object$coefficients,beta.se,beta.tval,2*pt(-abs(beta.tval),df=object$df))
  colnames(beta.TAB) <- c("Estimate","StdErr","t.value","p.value")
  
  phi.se <- sqrt(object$phi.var)
  phi.tval <- object$phi.coefficient/phi.se
  phi.TAB <- cbind(object$phi.coefficient,phi.se,phi.tval,2*pt(-abs(phi.tval),df=object$df))
  colnames(phi.TAB) <- c("Estimate","StdErr","t.value","p.value")
  
  
  res <- list(call=object$call,coefficients=beta.TAB,phi.coefficient=phi.TAB,
              deviance=object$deviance,df=object$df,iter=object$iter)
  
  class(res) <- "summary.BBreg"
  res
}


print.summary.BBreg <- function(x,...){
  cat("Call:\t")
  print(x$call)
  cat("\n")
  cat("Beta coefficients:\n")
  cat("\n")
  printCoefmat(x$coefficients,P.values=TRUE,has.Pvalue=TRUE)
  cat("\n---------------------------------------------------------------\n")
  cat("\nDispersion parameter coefficient (log):\n")
  cat("\n")
  printCoefmat(x$phi.coefficient,P.values=TRUE,has.Pvalue=TRUE)
  cat("\n---------------------------------------------------------------\n")
  
  cat("\nDeviance:",x$deviance," on ", x$df, " degrees of freedom\n") 

  cat("\nNumber of iterations in IWLS:", x$iter,"\n")
}