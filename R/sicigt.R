`sicigt` <-
function(x, nu = 1.0, kernel = "normal"){

    n         <- length(x)
    p         <- 2

    estimates <- switch(kernel,
                        "normal"   = mleigt(x, kernel = "normal"),
                        "t"        = mleigStNuFixed(x, nu),
                        "laplace"  = mleigt(x, kernel = "laplace"),
                        "logistic" = mleigt(x, kernel = "logistic")
                 )

    mu     <- estimates$muEstimate
    lambda <- estimates$lambdaEstimate

    f <- switch(kernel,
                "normal"   = digt(x, mu, lambda, nu = 1.0, 
                                  kernel = "normal", log = TRUE),
                "t"        = digt(x, mu, lambda, nu,       
                                  kernel = "t", log = TRUE),
                "laplace"  = digt(x, mu, lambda, nu = 1.0, 
                                  kernel = "laplace", log=TRUE),
                "logistic" = digt(x, mu, lambda, nu = 1.0, 
                                  kernel = "logistic", log=TRUE)
         )

    logLikelihood <- sum(f)
    sicResult     <- (-logLikelihood / n) + ((p / 2) * (log(n) / n))
    return(sicResult)
}

