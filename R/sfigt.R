`sfigt` <-
function(x,
                  mu     = 1.0,
                  lambda = 1.0,
                  nu     = 1.0,
                  kernel = "normal"){

    reliability <- switch(kernel,
                          "normal"   = 1 - pigt(x, mu, lambda, nu = 1.0, 
                                                "normal"),
                          "t"        = 1 - pigt(x, mu, lambda, nu, "t"),
                          "laplace"  = 1 - pigt(x, mu, lambda, nu = 1.0, 
                                                "laplace"),
                          "logistic" = 1 - pigt(x, mu, lambda, nu = 1.0, 
                                                "logistic")
                   )
    return(reliability)
    }

