#' @title Generate binary data with exchangeable correlated structure
#' @description
#'   Equivalent to cBern(n, p, rho, type="exchange")
#'
#' @param n number of observations
#' @param p the vector of marginal probabilities with dimension m
#' @param rho a non-negative value indecating the shared correlation coefficient
#' @return an n*p matrix of binary data
#' @examples
#' X <- cBernEx(10, rep(0.5,3), 0.5)
#' @export
#'
#'
cBernEx <- function(n, p, rho){
  #Generate Correlated Bernoulli Distribution

  if(!is.atomic(p) || typeof(p)!='double') {
    warning("Invalid input of p")
    return(NaN)
  }
  if(sum((p<=0) | (p>=1))!=0) {
    warning("Invalid input of p")
    return(NaN)
  }

  m<-length(p)
  minP<-min(p)
  maxP<-max(p)
  rhoLimit<-sqrt((minP/(1-minP))/(maxP/(1-maxP)))
  rhoLimit1 <- floor(rhoLimit*10000)/10000
  if((rho<0) || (rho>rhoLimit)){
    message(paste('The non-negative Prentice constraint for rho is [',0,',',rhoLimit1,']', sep=''))
    warning('rho is out-of-range\n')
    return(NaN)
  }
  Pc<-sqrt(minP*maxP)/(sqrt(minP*maxP)+sqrt((1-minP)*(1-maxP)))
  Pa<-sqrt(rho*p*(1-p)/(Pc*(1-Pc)))
  Pb<-(p-Pa*Pc)/(1-Pa)
  if(rho==rhoLimit){
    Pb[which.max(p)] <- 1
    Pb[which.min(p)] <- 0
  }

  X<-replicate(n, {
    U<-rbinom(m, 1, Pa)
    Y<-rbinom(m, 1, Pb)
    Z<-rbinom(1, 1, Pc)
    (1-U)*Y+U*Z
  })
  X<-t(X)

  return(X)
}
# cBernEx(10,rep(0.5,5),0.5)
#cBernEx(10,c(0.3,0.5,0.7),0.4285)
