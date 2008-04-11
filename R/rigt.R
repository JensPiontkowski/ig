`rigt` <-
function(n,
                 mu     = 1.0,
                 lambda = 1.0,
                 nu     = 1.0,
                 kernel = "normal"){

    if(n == 0)      stop("Value of n must be greater or equal then 0")
    if(mu <= 0)     stop("mu must be positive")
    if(lambda <= 0) stop("lambda must be positive")

    v0 <- switch(kernel,
                 "normal"   = rchisq(n, 1),
                 "t"        = rf(n, 1, nu),
                 "laplace"  = rgamma(n, 1, 1/2, 2),
                 "logistic" = rbeta(n, 1, 1)
          )

    x1     <- mu + (((mu ^ 2) * v0) / (2 * lambda)) -
              (mu / (2 * lambda)) * sqrt(4 * mu * lambda * v0 +
              ((mu ^ 2) * (v0 ^ 2)))
    x2     <- (mu ^ 2) / x1
    p0     <- mu / (mu + x1)
    u0     <- runif(n)
    random <- rep(0, n)

    for(i in 1:n){

        if(u0[i] <= p0[i]){
            random[i] <- x1[i]
        }
        else
        {
            random[i] <- x2[i]
        }
    }
    return(random)
}

