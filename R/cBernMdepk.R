cBernMdepk <- function(n, p, rho, k){
  m <- length(p)
  X <- cBernMdepk_single(m,p, rho,k)

  if(length(which(is.na(X)))>0){
    warning("Invalid Input: Please adjust the input parameters.")
  }
  else{
    simX <- t(replicate(n,cBernMdepk_single(m,p, rho,k)))
    return(simX)
  }
}
# set.seed(100)
# #cor(cBernMdepk(10000,p,rho,k))
# p <- rep(0.6,4)
# n <- 100
# rho <- list()
# rho[[1]] <- c(0.4,0.3,0.2)
# rho[[2]] <- c(0.1,0.1)
# rho[[3]] <- c(0.05)
# #cBernMdepk_single(4,p,rho,k=3)
# X <- cBernMdepk(n,p,rho,k=3)
# cor(X)
# apply(X,2,mean)
#
#
#


