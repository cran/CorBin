#' To calculate the maximal allowed correlations
#' max for using cBern1dep to generate binary data
#' with 1-dependent structure
#'
#' @param p the vector of marginal probabilities with dimension m
#'
#' @return an (m-1)-dimensional vector rho, which is the maximum
#' the correlation between the adjacent variables
#' @export
#'
rhoMax1dep <- function(p){
  m <- length(p)
  if(m==1){
    return(1)
  }else{
  rholimit <- rep(1,m-1)

  rholimit[1] <- sqrt(p[2]*(1-p[1])/p[1]/(1-p[2]))
  for (i in 2:(m-1)){
    rholimit[i] <- rhoMaxMdep1(p[i-1],p[i],p[i+1])
  }
  return( rholimit )}
}

#sqrt(p[i]*(1-p[i+1])/p[i+1]/(1-p[i]))
