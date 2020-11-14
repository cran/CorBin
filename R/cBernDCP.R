#' @title  Generate binary data with decaying-product correlated structure
#' @description
#' Equivalent to cBern(n, p, rho, type="DCP")
#'
#' @param n number of observations
#' @param p the vector of marginal probabilities with dimension m
#' @param rho either a non-negative value indecating the shared correlation coefficient
#' or and m-1 vector indicating the correlation coefficients between adjacent
#' variables.
#' @examples
#' X <- cBernDCP(10, rep(0.5,3), c(0.2,0.2))
#' @return an n*p matrix of binary data
#' @export
#'
cBernDCP <- function(n, p, rho){
  m <- length(p)
  if(sum((p<=0) | (p>=1))>0) {
    warning("The range of p should be (0, 1).\n")
    return(NaN)
  }
  if((length(rho)!=1)&(length(rho)!=(m-1))){
    warning("Invalid Input: The length of rho has to be 1 or m-1.\n")
    return(NaN)
  }
  if(length(rho)==1){
    rho <- rep(rho,max(1,m-1))
  }
  rholimit <- rhoMaxDCP(p)
  rholimit1 <- min(rholimit)
  rholimit2 <- floor(rholimit1*10000)/10000
  temp <- rholimit-rho
  if(length(which(temp<0))>0){
    message(paste('The non-negative Prentice constraint for rho is [',0,',',rholimit2,']', sep=''))
    message('For detailed valid range, please use rhoMaxAR(p) to get the upper bound of each element in rho.')
    warning('rho is out-of-range')
    return(NaN)
  }
  else{
    simX <- t(replicate(n,cBernAR_single(m,p, rho)))
    return(simX)
  }
}
#cor(cBernAR(10000,c(0.5,0.6,0.5,0.3),0.6))
