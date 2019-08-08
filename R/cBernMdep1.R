cBernMdep1 <- function(n, p, rho){
  m <- length(p)
  if((length(rho)!=1)&(length(rho)!=m-1)){
    message(paste0("The length of rho has to be 1 or ",m-1,"."))
    return(NaN)
  }
  else{
    if(length(rho)==1){
      rho <- rep(rho,m-1)
    }
    rholimit <- rhoMax1dep(p)

    rholimit1 <- min(rholimit)
    rholimit2 <- floor(rholimit1*10000)/10000
    ## to see if the giving rho is valid
    temp <- rholimit-rho
    if(length(which(temp<0))>0){
      message(paste('The smallest valid range of rho is [',0,',',rholimit2,']', sep=''))
      message('rho is out-of-range\n')
      message('For detailed valid range, please use rhoMaxMdep(p) to get the accuarate upbound of each element in rho.\n')
     return(NaN)
    }
    else{
     simX <- t(replicate(n,cBernMdep1_single(m,p, rho)))
     return(simX)
   }
  }
}
# pp <- cBernMdep1(10,rep(0.6,5),rep(0.4,2))
# # pp <- cBernMdep1(1,rep(0.5,5),0.8)
# # rhoMaxMdep(p)
# pp <- cBernMdep1(100000,rep(0.5,5),rep(0.1,4))
# cor(pp)
