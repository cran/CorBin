#' @title Main function
#' @description The main function of our package, through which we can simulate
#' correlated binary data under different settings.
#'
#' @param n number of observations
#' @param p the vector of marginal probabilities with dimension m
#' @param rho For the first three types, rho is either a non-negative value indecating the shared correlation coefficient
#' or and m-1 vector indicating the correlation coefficients between adjacent
#' variables. For the general case, rho should be a list, the i-th element of which specifies the coefficients on the i-th minor diagnal.
#' @param type including 4 types.
#'
#' type="exchange"
#'
#' type="DCP"
#'
#' type="1-dependent"
#'
#' type="General"
#'
#' @param k (for 'General' use only). The number of layers setting for k-dependent structure. k=m-1 for the general case.

#' @return an n*p matrix of binary data
#' @importFrom stats rbinom
#' @seealso \code{\link{cBernEx}}, \code{\link{cBernDCP}}, \code{\link{cBern1dep}}
#' @export
#'
#' @references
#' Jiang, W., Song, S.,  Hou, L. and Zhao, H. A set of efficient methods to generate high-dimensional binary data with specified correlation structures. \emph{The American Statistician}. DOI:10.1080/00031305.2020.1816213
#'
#' @examples
#' X <- cBern(10, rep(0.5,3), 0.5, type="exchange")
#'
#' X <- cBern(10, rep(0.5,3), c(0.2,0.2), type="DCP")
#'
#' X <- cBern(5, c(0.4,0.5,0.6), c(0.2,0.3), type="1-dependent")
#'
#' rho <- list()
#' rho[[1]] <- c(0.2,0.3)
#' rho[[2]] <- 0.1
#' X <- cBern(2, c(0.7,0.8,0.9),rho=rho,type="General", k=2)

cBern <- function(n, p, rho, type, k=NULL){
 # options(warn=-1)
  if(type=="exchange"){
    return(cBernEx(n, p, rho))
  }
  else if(type=="DCP"){
    return(cBernDCP(n, p, rho))
  }
  else if(type=="1-dependent"){
    return(cBern1dep(n, p, rho))
  }
  else if(type=="General"){
    return(cBernMdepk(n, p, rho, k))
  }
  else{
    warning("The type should be 'exchange','DCP','1-dependent' or 'General'\n")
    #return(NaN)
  }
}
# a <- cBern(10,c(0.2,0.5,0.4),rho=0.5,type = "exchange")
# cor(cBern(10000,c(0.2,0.5,0.4),rho=0.5,type = "exchange"))
