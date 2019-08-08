cBernMdepk_single <- function(m, p, rho, k){
  for(w in 1:k){
    p[m+w] <- p[m]
    rho[[w]][(m-w+1):m] <- 0
  }

  Y <- b <- matrix(0,k,m)
  for(w in 1:k){
    for(j in 1:m){
      b[w,j] <- p[j]*p[w+j]/
        (rho[[w]][j]*sqrt(p[j]*p[w+j]*(1-p[j])*(1-p[w+j]))+p[j]*p[w+j])
      Y[w,j] <- rbinom(1,1,b[w,j])
      }
  }
  a <- U <- X <- rep(0,m)
  for(w in 1:m){
    prod1 <- prod2 <-  1
    if(w==1){
      for(l in 1:k){
        prod1 <- prod1*b[l,w]
        prod2 <- prod2*Y[l,w]
      }
    }
    else if(w<=k){
      for(l in 1:k){
        prod1 <- prod1*b[l,w]
        prod2 <- prod2*Y[l,w]
      }
      for(l in 1:(w-1)){
        prod1 <- prod1*b[l,w-l]
        prod2 <- prod2*Y[l,w-l]
      }
    }
    else{
      for(l in 1:k){
        prod1 <- prod1*b[l,w]*b[l,w-l]
        prod2 <- prod2*Y[l,w]*Y[l,w-l]
      }
    }
    a[w] <- p[w]/prod1
    U[w] <- rbinom(1,1,a[w])
    X[w] <- U[w]*prod2
  }
  return(X)
}
#cBernMdep_single(m, p, rho, k)




#
#
# rho <- list()
# rho[[1]] <- rep(0.3,3)
# p <- rep(0.2,3)
# k <- 1
#
# m <- 3
