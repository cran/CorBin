#' @title Generate binary data with 1-dependent correlated structure
#' @description
#' Equivalent to cBern(n, p, rho, type="1-dependent")
#'
#' @param n number of observations
#' @param p the vector of marginal probabilities with dimension m
#' @param rho either a non-negative value indecating the shared correlation coefficient
#' or and m-1 vector indicating the correlation coefficients between adjacent
#' variables.
#'
#' @return an n*p matrix of binary data
#' @examples
#' X <- cBern1dep(5, c(0.4,0.5,0.6), c(0.2,0.3))
#' @export
#'
cBern1dep <- function(n, p, rho){

  m <- length(p)
  if((length(rho)!=1)&(length(rho)!=(m-1))){
    warning("Invalid Input: The length of rho has to be 1 or m-1.\n")
    return(NaN)
  }

  res <- cBernMdep0(n,p,rho)
  if(!is.na(res[1])){
    message("Worked out!")
    return (res)
  }
  else{
    message("Now trying UYY method (Method 1).")
    res <- cBernMdep1(n,p,rho)
    if(!is.na(res[1])){
      message("Worked out!")
      return(res)
    }
    else{
      warning("Invalid Input: Please adjust the setting of rho according to the instructions.")
    }
  }
}
# a <- cBernMdep(1,c(0.3,0.5,0.7,0.6),rep(0.328,3))
# hhwarnings()
