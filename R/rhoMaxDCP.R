#' For calculating the maximal allowed correlations
#' max for binary data with decaying-product structure.
#'
#' @param p marginal probabilities
#'
#' @return an (m-1)-dimensional vector rho, which is the maximum
#' the correlation between the adjacent variables
#' @export
#'

rhoMaxDCP <- function(p){
  m <- length(p)
  if(m==1){
    return(1)
  }else{
  rholimit <- rep(1,m-1)
  for (i in 1:(m-1)){
    p1 <- min(p[i],p[i+1])
    p2 <- max(p[i],p[i+1])
    rholimit[i] <- sqrt(p1*(1-p2)/(1-p1)/p2)
  }
  return(rholimit)
}}
# rhoMaxAR(c(0.5,0.6,0.5,0.3))


# rhoMaxAR2 <- function(p){
#   m <- length(p)
#   rholimit <- rep(1,m-1)
#   for (i in 1:(m-1)){
#     rholimit[i] <- sqrt(p[i+1]*(1-p[i+1])/p[i]/(1-p[i]))
#   }
#   return(rholimit)
# }
# rhoMaxAR2(c(0.5,0.6,0.5,0.3))
