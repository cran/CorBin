cBernAR_single <- function(m, p, rho){
  #Generate Correlated Bernoulli Distribution
  # p should have m elements

  # if(!is.atomic(p) || typeof(p)!='double') return(NaN)

  a <- b <- Y <- X <- U <- rep(0,m)
  X[1] <- Y[1] <- rbinom(1,1,p[1])
  # thetalimit[1] <- 1
  # for (i in 1:(m-1)){
  #   thetalimit[i] <- sqrt(p[i+1]*(1-p[i-1])/p[i]/(1-p[i]))
  # }
  # ThetaLimit<- min(thetalimit)
  # if((rho<0) || (rho>ThetaLimit)){
  #   cat(paste('The range of theta is [',0,',',round(ThetaLimit,3),']\n', sep=''))
  #   cat('rho is out-of-range\n')
  #   return(NaN)
  # }

  for(i in 2:m){
    a[i] <- sqrt(p[i]*(1-p[i])/p[i-1]/(1-p[i-1])) * rho[i-1]
    b[i] <- (p[i]-a[i]*p[i-1])/(1-a[i])
    U[i] <- rbinom(1,1,a[i])
    Y[i] <- rbinom(1,1,b[i])
    X[i] <- (1-U[i])*Y[i]+U[i]*X[i-1]
  }
  return(X)
}

