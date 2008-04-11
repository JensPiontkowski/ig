`qigt` <-
function(p,
                 mu         = 1.0,
                 lambda     = 1.0,
                 nu         = 1.0,
                 kernel     = "normal",
                 lower.tail = TRUE,
                 log.p      = FALSE){

    auxiliary <- function(p, mu, lambda, nu, kernel = "normal"){

        origin        <- p
        m             <- mu
        l             <- lambda
        n             <- nu
        kerneldensity <- switch(kernel,
                                "normal"   = "normal",
                                "t"        = "t",
                                "laplace"  = "laplace",
                                "logistic" = "logistic"
                         )
        f <- function(x, p = origin, mu = m, lambda = l, nu = n, 
                      kernel = kerneldensity){

            results <- switch(kernel,
                              "normal"   = pigt(x, mu, lambda, nu = 1.0, 
                                                kernel = "normal")   - p,
                              "t"        = pigt(x, mu, lambda, nu,       
                                                kernel = "t")        - p,
                              "laplace"  = pigt(x, mu, lambda, nu = 1.0, 
                                                kernel = "laplace")  - p,
                              "logistic" = pigt(x, mu, lambda, nu = 1.0, 
                                                kernel = "logistic") - p
                       )
            return(results)
        }

        optimitation <- uniroot(f, interval = c(0.000001, 5000))
        pquantile    <- optimitation$root
        return(pquantile)
    }

    pquantiles <- switch(kernel,
                         "normal"   = mapply(auxiliary, p, mu, lambda, 
                                             nu = 1.0, "normal"),
                         "t"        = mapply(auxiliary, p, mu, lambda, 
                                             nu, "t"),
                         "laplace"  = mapply(auxiliary, p, mu, lambda, 
                                             nu = 1.0, "laplace"),
                         "logistic" = mapply(auxiliary, p, mu, lambda, 
                                             nu = 1.0, "logistic")
                  )
    return(pquantiles)
}

