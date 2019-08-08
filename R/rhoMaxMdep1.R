## input : p[i-1], p[i], p[i+1]
## return : the maximum of rho

rhoMaxMdep1 <- function(pMinus1,p,pPlus1){
  a <- sqrt(1-p)*sqrt(1-pPlus1)*sqrt(1-pMinus1)
  b <- sqrt((1-pPlus1)*p*pMinus1)+sqrt((1-pMinus1)*p*pPlus1)
  c <- -sqrt(pMinus1*pPlus1)*sqrt(1-p)
  #s1[i] <- (-b-sqrt(b**2-4*a*c))/2/a
  s2 <- (-b+sqrt(b**2-4*a*c))/2/a
  return(s2)
}


