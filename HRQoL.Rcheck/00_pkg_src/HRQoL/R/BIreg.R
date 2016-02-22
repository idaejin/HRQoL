BIreg <- function(formula,n,data=list(),disp=FALSE){
  
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
  
  mf <- model.frame(formula=formula,data=data)
  x <- model.matrix(attr(mf,"terms"),data=mf)
  y <- model.response(mf)
  
  m <- length(y)
    
  if (class(y)=="factor"){
    stop("The response variable y must be numeric")
  }
  
  if (min(y==as.integer(y))==0){
    stop("The response variable y must be integer")
  } 
  
  if (max(y)>n | min(y) < 0){
    stop("The response variable y must be bounded between 0 and n")
  }
  
  # Calculing the coefficients
  test <- IWLS(y,x,n)
  coef <- test$beta
  df <- nrow(x)-ncol(x)
  vcov <- test$vcov
  colnames(vcov) <- rownames(vcov) <- colnames(x)
  
  est <- list(coefficients=coef)
  
  # Compute the overdispersion 
  numpara <- length(est$coefficients)
  h <- (1/(1+exp(-as.vector(x%*%est$coefficients))))
  mu <- n*h
  
  if (disp==FALSE){
    phi=1}
  else{
    phi <- sum((y-mu)^2/(n*h*(1-h)))/(length(y)-numpara)
  }
  
  est$vcov <- phi*vcov
  est$phi <- phi
  
  
  # Calculing the fitted values, residuals and degrees of freedom
  est$fitted.values <- h
  est$residuals <- y-mu
  est$df <- df
  
  # Calculating deviance / escaled deviance
  e <- sum(y)/(length(y))
  
  i0 <- which(y==0)
  y0 <- y[i0]
  mu0 <- mu[i0]
  Deviance0 <- sum((n-y0)*log((n-y0)/(n-mu0)))
  nullDeviance0 <- sum((n-y0)*log((n-y0)/(n-e)))
  
  i1 <- which(y==n)
  y1 <- y[i1]
  mu1 <- mu[i1]
  Deviance1 <- sum(y1*log(y1/mu1))
  nullDeviance1 <- sum(y1*log(y1/e))
  
  i2 <- which(y<n & y>0)
  y2 <- y[i2]
  mu2 <- mu[i2]
  Deviance2 <- sum((n-y2)*log((n-y2)/(n-mu2))+y2*log(y2/mu2))
  nullDeviance2 <- sum((n-y2)*log((n-y2)/(n-e))+y2*log(y2/e))
  
  est$deviance <- 2*(Deviance0+Deviance1+Deviance2)
  est$df.null <- m-1
  est$null.deviance <- 2*(nullDeviance0+nullDeviance1+nullDeviance2)
  # scaled deviance
  if (disp==TRUE){
    est$deviance <- est$deviance/est$phi
    est$null.deviance <- est$null.deviance/est$phi
  }
  
  # Number of iterations in IWLS
  est$iter <- test$iter
  
  # Model matrix and the dependent variable
  est$X <- x
  est$y <- y
  est$n <- n
  est$noObs <- m
  
  class(est) <- "BIreg"
  
  est$call <- match.call()
  est$formula <- formula
  
  est
}

print.BIreg <- function(x,...){
  cat("Call:")
  print(x$call)
  cat("\nCoefficients:\n")
  print(t(x$coefficients))
  if (x$phi!=1){
    cat("\nDispersion parameter:",x$phi,"\n")
    cat("\nDegrees of freedom:", x$df.null, "Total ;", x$df, "Residual")
    cat("\nScaled null deviance:",x$null.deviance)
    cat("\nScaled deviance:",x$deviance)
  }else{
    cat("\nDegrees of freedom:", x$df.null, "Total ;", x$df, "Residual")
    cat("\nNull deviance:",x$null.deviance)
    cat("\nDeviance:",x$deviance)
  }
}


summary.BIreg <- function(object,...){
  se <- sqrt(diag(object$vcov))
  tval <- object$coefficients/se
  TAB <- cbind(object$coefficients,se,tval,2*pt(-abs(tval),df=object$df))
  colnames(TAB) <- c("Estimate","StdErr","t.value","p.value")
  
  null.df <- object$df.null
  df <- object$df
  
  Chi <- object$null.deviance-object$deviance
  Chi.p.value <- 1-pchisq(Chi,null.df-df)
  
  res <- list(call=object$call,coefficients=TAB,phi=object$phi,
              deviance=object$deviance,df=df,null.deviance=object$null.deviance,df.null=null.df,
              chi.value=Chi,chi.p.value=Chi.p.value,iter=object$iter)
  
  class(res) <- "summary.BIreg"
  res
}


print.summary.BIreg <- function(x,...){
  cat("Call:\n")
  print(x$call)
  cat("\n")
  
  printCoefmat(x$coefficients,P.values=TRUE,has.Pvalue=TRUE)  
  if (x$phi!=1){
    cat("\nDispersion parameter:",x$phi,"\n")
    cat("\nScaled Null deviance:",x$null.deviance," on ",x$df.null, "degrees of freedom")
    cat("\nScaled deviance:",x$deviance," on ", x$df, "degrees of freedom") 
    cat("\nDeviance test:",x$chi.p.value,"\n")
  }else{
    cat("\nNull deviance:",x$null.deviance," on ",x$df.null, "degrees of freedom")
    cat("\nDeviance:",x$deviance," on ", x$df, "degrees of freedom") 
    cat("\nDeviance test:",x$chi.p.value,"\n")
  }
  cat("\nNumber of iterations in IWLS:", x$iter)
  
}


predict.BIreg <- function(object,newdata=NULL,...){
  
  if(is.null(newdata))
    y <- fitted(object)
  else{
    if(!is.null(object$formula)){
      x <- model.matrix(object$formula,newdata)
    } else{
      x <- newdata
    }
    y <- as.vector(x%*%coef(object))
  }
  y
}