`digt` <-
function(x,
                 mu     = 1.0,
                 lambda = 1.0,
                 nu     = 1.0,
                 kernel = "normal",
                 log    = FALSE){

    quantity <- sqrt(lambda / mu) * (sqrt(x / mu) - sqrt(mu / x))
    jacobian <- sqrt(lambda) / sqrt(x ^ 3)
    density  <- switch(kernel,
                       "normal"   = dnorm(x = quantity, 0, 1) * jacobian,
                       "t"        = dt(x = quantity, df = nu) * jacobian,
                       "laplace"  = dlaplace(x = quantity)    * jacobian,
                       "logistic" = dlogis(x = quantity)      * jacobian
                )

    if(log == TRUE){
        density <- log(density)
    }
    return(density)
}

