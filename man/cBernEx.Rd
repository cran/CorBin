% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cBernEx.R
\name{cBernEx}
\alias{cBernEx}
\title{Generate binary data with exchangeable correlated structure}
\usage{
cBernEx(n, p, rho)
}
\arguments{
\item{n}{number of observations}

\item{p}{the vector of marginal probabilities with dimension m}

\item{rho}{a non-negative value indecating the shared correlation coefficient}
}
\value{
an n*p matrix of binary data
}
\description{
Equivalent to cBern(n, p, rho, type="exchange")
}
\examples{
X <- cBernEx(10, rep(0.5,3), 0.5)
}
