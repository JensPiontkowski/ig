`hfigt` <-
function(x,
                  mu     = 1.0,
                  lambda = 1.0,
                  nu     = 1.0,
                  kernel = "normal"){

    hazard <- switch(kernel,
                "normal"   = {
                             digt(x, mu, lambda, nu = 1.0, "normal") /
                             (1 - pigt(x, mu, lambda, nu = 1.0, "normal"))
                             },
                "t"        = {
                             digt(x, mu, lambda, nu, "t") /
                             (1 - pigt(x, mu, lambda, nu, "t"))
                             },
                "laplace"  = {
                             digt(x, mu, lambda, nu = 1.0, "laplace") /
                             (1 - pigt(x, mu, lambda, nu = 1.0, "laplace"))
                             },
                "logistic" = {
                             digt(x, mu, lambda, nu = 1.0, "logistic") /
                             (1 - pigt(x, mu, lambda, nu = 1.0, "logistic"))
                             }
              )
    return(hazard)
}

