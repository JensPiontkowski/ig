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
                 normal = rnorm(n,0, 1), 
                 t = rt(n, nu, ncp = 0),
                 laplace = sqrt(2*rexp(n))*rnorm(n), 
                 logistic = rlogis(n, 0, 1))
    v0 <- v0^2
    x1 <- mu + (((mu^2) * v0)/(2 * lambda)) - (mu/(2 * lambda)) * 
      sqrt(4 * mu * lambda * v0 + ((mu^2) * (v0^2)))
    x2 <- (mu^2) / x1
    p0 <- mu / (mu + x1)
    u0 <- runif(n)
    random <- x2
    positions_x1<-(u0<=p0)
    random[positions_x1]<-x1[positions_x1]
    return(random)
}

