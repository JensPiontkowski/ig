`loglikigt` <-
function(x, nu = 1.0, kernel = "normal"){

    estimates <- switch(kernel,
                        "normal"   = mleigt(x, kernel = "normal"),
                        "t"        = mleigSt(x),
                        "laplace"  = mleigt(x, kernel = "laplace"),
                        "logistic" = mleigt(x, kernel = "logistic")
                 )

    mu     <- estimates$muEstimate
    lambda <- estimates$lambdaEstimate
    f      <- switch(kernel,
                     "normal"   = digt(x, mu, lambda, nu = 1.0, 
                                       kernel = "normal", log = TRUE),
                     "t"        = digt(x, mu, lambda, nu = nu,  
                                       kernel = "t", log = TRUE),
                     "laplace"  = digt(x, mu, lambda, nu = 1.0, 
                                       kernel = "laplace", log = TRUE),
                     "logistic" = digt(x, mu, lambda, nu = 1.0, 
                                       kernel = "logistic", log = TRUE)
              )    
     
    logLikelihood <- sum(f)
    return(logLikelihood)
}

