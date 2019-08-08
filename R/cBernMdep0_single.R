cBernMdep0_single <- function(m, p, rho, r){
  U <- Y <- rep(0,m)
  p1 <- max(p)
  a <- p/p1
  A <- rbinom(m,1,a)
  Y <- rbinom(m,1,p1)
  X <- rep(0,m)
  X[1] <- Y[1]
  #r[1] <- 0
  U[1] <- rbinom(1,1,r[1])
  if(m == 1){
    return(X)
  }
  else{
    for(i in 2:m){
      #r[i] <- rho[i-1]/(1-r[i-1])
      U[i] <- rbinom(1,1,r[i])
      X[i] <- (1-U[i])*Y[i] + U[i]*Y[i-1]
    }  
  }
  W <- A*X
  return(W)
} 
#cBernMdep0_single(10,0.5,runif(10,0,1))


