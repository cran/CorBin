#' For calculating the maximal allowed correlation
#' coefficient for binary data with exchangeable structure.
#'
#' @param p the vector of marginal probabilities with dimension m
#'
#' @return the maximal allowed correlation coefficient
#' @export
#'
rhoMaxEx <- function(p){
  return(sqrt(min(p)*(1-max(p))/max(p)/(1-min(p))))
}
