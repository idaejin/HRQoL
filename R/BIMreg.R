BIMreg <- function(formula,n,random,data=list()){
  
  #Random effects
  if (class(random)=="character"){
  }else{
    stop("The random parameter must be a character")
  }
    
  #Number of random effects
  nrand <- length(random)
  
  for (l in 1:nrand){
    if (random[l] %in% names(data)){
    }else{
      stop("The selected random component is not in the data frame")
    }
    
    if (class(data[,random[l]])=="factor"){
    }else{
      stop("The random components must be factors")
    }
  }
  
  #Number of trials
  if (n==as.integer(n)){
  } else {
    stop("The number of trials n must be integer")
  }
  
  if (n<=0){
    stop("The number of trials n must be positive")
  }

  
  #Defining the response variable and models matrix for fixed effect
  fix.mf <- model.frame(formula, data=data)
  X <- model.matrix(attr(fix.mf, "terms"), data=fix.mf)
  y <- model.response(fix.mf)
  
  #Response variable
  if (min(y==as.integer(y))==0){
    stop("The response variable y must be integer")
  } 
  
  if (max(y)>n | min(y) < 0){
    stop("The response variable y must be bounded between 0 and n")
  }
  
  
  # Number of observations
  nObs <- dim(X)[1]
  

  #Number of cluster in each random efect
  m <- NULL
  for (l in 1:nrand){
    m <- cbind(m,length(levels(data[,random[l]])))
  }

  #Geting the random effects in a matrix
  K <- NULL
  for (l in 1:nrand){
    K <- cbind(K,data[,random[l]])
  } 
  
  # Defining Z
  Z <- NULL
  for (l in 1:nrand){
    
    Z1 <- matrix(nrow = nObs,ncol=m[l],0)
    
    for (i in 1:nObs){
      for (j in 1:m[l]){
        if (K[i,l]==levels(as.factor(K[,l]))[j]){
          Z1[i,j] <- 1   
        }
      }
    }
    Z <- cbind(Z,Z1)
  }

  
  # initial values
  iter=0
  while.iter <- 0
  if (dim(X)[2]==1){
    beta <- BIreg(y~1,n)$coef
  } else{
    beta <- BIreg(y~X[,-1],n)$coef
  }
  
  oldbeta <- rep(Inf,dim(X)[2]) 
  sig <- rep(1,nrand)
  oldsig <- rep(Inf,nrand)

  oldu <- rep(Inf,sum(m))
  u <- rep(1,sum(m))
  
  eta <- X%*%beta
  
  while (max(abs(oldsig-sig))>0.001){
    
    oldsig <- sig
    oldbeta <- rep(Inf,dim(X)[2])
    oldu <- u
    
    while (max(abs(beta-oldbeta))>0.001){
      
      oldbeta <- beta

      # Relevant to the working vector
      p <- 1/(1+exp(-(eta)))
      mu <- n*p
      g. <- n/(mu*(n-mu))
      yw <- eta + (g.*(y-mu))
      
      W <- diag(c((n*p*(1-p))))
      
      m. <- 0
      m1 <- 0    
      m2 <- 0
      u. <- NULL
      
    for (l in 1:nrand){
      m1 <- m.+1
      m2 <- m.+m[l]
      
      D. <- (1/sig[l])*diag(m[l])
        
      if (nrand >1){
         u.. <- solve(t(Z[,seq(m1,m2)])%*%W%*%Z[,seq(m1,m2)]+D.)%*%t(Z[,seq(m1,m2)])%*%W%*%(yw-X%*%beta-Z[,-seq(m1,m2)]%*%u[-seq(m1,m2)])
      } else {
      u.. <- solve(t(Z[,seq(m1,m2)])%*%W%*%Z[,seq(m1,m2)]+D.)%*%t(Z[,seq(m1,m2)])%*%W%*%(yw-X%*%beta)
      }
      
        u. <- c(u.,u..)
        
        m. <- m.+m[l]
      }
      
      u <- u.
      
      beta <- solve(t(X)%*%W%*%X)%*%t(X)%*%W%*%(yw-Z%*%u)
      
      eta <- X%*%beta+Z%*%u       
    }
    
    # Calculating the variance component sigmau
    m. <- 0
    m1 <- NULL    
    m2 <- NULL
    sig. <- NULL
    for (l in 1:nrand){
      m1 <- m.+1
      m2 <- m.+m[l]
      
      D. <- (1/sig[l])*diag(m[l])
      
      sig.. <- as.numeric((1/m[l])*(t(u[seq(m1,m2)])%*%u[seq(m1,m2)]+sum(diag(solve(t(Z[,seq(m1,m2)])%*%W%*%Z[,seq(m1,m2)]+D.)))))
      sig. <- cbind(sig.,sig..)
      
      m. <- m.+m[l]
    }
    sig <- sig.
    
    iter=iter+1
  }
  # Returning the final values
  
  d. <- NULL
  for (l in 1:nrand){
    d.. <- (1/sig[l])*rep(1,m[l])
    d. <- c(d.,d..)
  }
  D. <- diag(d.)
  
  V <- solve(W)+Z%*%solve(D.)%*%t(Z)
  # Beta
  coefficients <- beta
  vcov <- solve(t(X)%*%solve(V)%*%X)
  
  # Random efffects u
  random.var <- sig
  colnames(random.var) <- random 
  random.se <- sqrt(random.var)
  random <- u
  
  # fitted values and residuals
  fitted <- 1/(1+exp(-eta))
  residuals <- c(y-n*fitted)
  
  # Deviance
  # See Pawitan book
  # deviance1 <- 2*log(2*pi)+log(det(solve(W)))+t(yw-eta)%*%W%*%(yw-eta)+log(det(solve(D.)))+t(u)%*%D.%*%u+log(det(t(Z)%*%W%*%Z+D.))
  # Breslow's approach
  deviance <- log(det(V))+log(det(t(X)%*%solve(V)%*%X))+t(yw-X%*%beta)%*%solve(V)%*%(yw-X%*%beta)
  
  
  # output 
  out <- list(coefficients=coefficients,coef.vcov=vcov,
              random.coef=random,random.var=random.var,
              fitted.values=fitted,residuals=residuals,
              random.se=sqrt(random.var),working=yw,deviance=deviance,
              iter=iter,nObs=nObs,nrand=nrand,cluster=m,
              y=y,X=X,Z=Z,D=solve(D.),n=n,W=W)
  
  
  class(out) <- "BIMreg"
  
  out$call <- match.call()
  out$formula <- formula
  
  out
}



print.BIMreg <- function(x,...){
  cat("Call:")
  print(x$call)
  
  cat("\nFixed effects:")
  cat("\nCoefficients:\n")
  print(t(x$coefficients))
  cat("\nRandom effects:")
  cat("\nNumber of random components:",x$nrand,"\n")
  cat("Number of groups in each random component:",x$cluster)
  cat("\nStd. Error(s):",x$random.se,"\n")
}

# fitted.BIMreg <- function(x,...){
#   x$fitted
# }
# 
# residuals.BIMreg <- function(x,...){
#   x$residuals
# }



summary.BIMreg <- function(object,...){
  
  # Fixed
  se <- sqrt(diag(object$coef.vcov))
  tval <- object$coefficients/se
  TAB <- cbind(object$coefficients,se,tval,2*pnorm(-abs(tval)))
  colnames(TAB) <- c("Estimate","StdErr","t.value","p.value")
  
  # Random
  p <- object$fitted.values
  V <- diag(c(1/(object$n*p*(1-p))))+object$Z%*%object$D%*%t(object$Z)
  V. <- solve(V)
  P <- V.- V.%*%object$X%*%object$coef.vcov%*%t(object$X)%*%V.
  Fisher.randvar <- -(1/2)*4*object$random.var*(sum(diag(P%*%object$Z%*%t(object$Z)%*%P%*%object$Z%*%t(object$Z))))
  
  var.randvar=-1/Fisher.randvar
  
  se.sdu <- sqrt(var.randvar)
  tval.sdu <- object$random.se/se.sdu
  pvalue.sdu <- 2*pnorm(-abs(tval.sdu))
  
  TAB.sdu <- cbind(t(object$random.se),t(se.sdu),t(tval.sdu),t(pvalue.sdu))
  colnames(TAB.sdu) <- c("Estimate","StdError","t.value","p.value")
  
  # output
  res <- list(call=object$call,coefficients=TAB,
              random=TAB.sdu,random.coef=object$random.coef,deviance=object$deviance,
              iter=object$iter,nObs=object$nObs,nrand=object$nrand,cluster=object$cluster)
  
  class(res) <- "summary.BIMreg"
  res
}

print.summary.BIMreg <- function(x,...){
  cat("Call:\t")
  print(x$call)
  
  
  cat("\nFixed effects coefficients:\n")
  cat("\n")
  printCoefmat(x$coefficients,P.values=TRUE,has.Pvalue=TRUE)
  cat("\n---------------------------------------------------------------\n")
  cat("\nRandom effects standard error coefficients:\n")
  cat("\n")
  printCoefmat(x$random,P.values=TRUE,has.Pvalue=TRUE)
  cat("\nNumber of random components:",x$nrand,"\n")
  cat("Number of clusters per random component:",x$cluster,"\n")
  cat("\n---------------------------------------------------------------\n")
  cat("\nNumber of observations:",x$nObs,"\n")
  cat("Deviance:",x$deviance,"\n")
  cat("\nNumber of iterations in IWLS:", x$iter,"\n")
  cat("\n")
}

predict.BIMreg <- function(object,newdata=NULL,...){
  
  if(is.null(newdata))
    y <- fitted(object)
  else{
    if(!is.null(object$formula)){
      x <- model.matrix(object$formula,newdata)
    } else{
      x <- newdata
    }
    y <- as.vector(x%*%coef(object)+object$Z%*%object$random.coef)
  }
  y
}
