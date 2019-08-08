
cBernMdep1_single <- function(m, p, rho){

    a <- b <- Y <- X <- U <- rep(0,m)
    # rholimit <- rep(1,m)
    X[1] <- Y[1] <- rbinom(1,1,p[1])


    b[1] <- sqrt(p[1]*p[2])/(sqrt(p[1]*p[2])+rho[1]*
                               sqrt((1-p[1])*(1-p[2])))
    a[1] <- p[1]/b[1]
    U[1] <- rbinom(1,1,a[1])
    Y[1] <- rbinom(1,1,b[1])
    X[1] <- U[1]*Y[1]

    for(i in 2:(m-1)){
      # print(i)
      b[i] <- sqrt(p[i]*p[i+1])/(sqrt(p[i]*p[i+1])+rho[i]*
                                   sqrt((1-p[i])*(1-p[i+1])))
      a[i] <- p[i]/b[i-1]/b[i]

      U[i] <- rbinom(1,1,a[i])
      Y[i] <- rbinom(1,1,b[i])
      X[i] <- U[i]*Y[i]*Y[i-1]
    }
    # print(b[1])
    # print(b[2])
    # print(a[2])

    a[m] <- b[m] <- sqrt(p[m]/b[m-1])
    U[m] <- rbinom(1,1,a[m])
    Y[m] <- rbinom(1,1,b[m])
    X[m] <- U[m]*Y[m]*Y[m-1]

    # if(minus==1){
    #   index <- seq(1,m,2)
    #   X[index] <- 1-X[index]
    # }
    return(X)
  }

# }
