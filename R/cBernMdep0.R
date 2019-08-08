cBernMdep0 <- function(n, p, rho){

  m <- length(p)
  if((length(rho)!=1)&(length(rho)!=(m-1))){
    message("The length of rho has to be 1 or m-1.")
    return(NaN)
  }
  message("First try AW method (Method 2).")
  ### calculate new rho according to giving p
  r <- rep(0,m)
  p1 <- max(p)
  a <- p/p1
  newrho <- rep(0,m-1)
  if(length(rho)==1){
    for(i in 1:(m-1)){
      newrho[i] <- rho/(sqrt(a[i]*a[i+1]/(1-a[i]*p1)/(1-a[i+1]*p1))*(1-p1))
    }

  }
  if(length(rho)>1){
    for(i in 1:(m-1)){
      newrho[i] <- rho[i]/(sqrt(a[i]*a[i+1]/(1-a[i]*p1)/(1-a[i+1]*p1))*(1-p1))
    }
  }
  ### calculate r

  r[1] <- 0
  if(m>1){
    for(i in 2:m){
      r[i] <- newrho[i-1]/(1-r[i-1])
    }
  }
  if(length(which((r<0)|(r>1)))>0){
    message("The AW method is not suitable for the giving p and n.")
    return(NaN)
  }
  else{
    simX <- t(replicate(n,cBernMdep0_single(m,p, newrho,r)))
    return(simX)
  }
}
# pp <- cBernMdep0(1000,c(0.3,0.5,0.7),rep(0.1,3))
# #pp
#  cor(pp)
