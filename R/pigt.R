`pigt` <-
function(q,
                 mu         = 1.0,
                 lambda     = 1.0,
                 nu         = 1.0,
                 kernel     = "normal",
                 lower.tail = TRUE,
                 log.p      = FALSE){
    
    cumulative <- function(value, m, l, n, k){

        integration <- integrate(digt,
                                 lower  = 0,
                                 upper  = value,
                                 mu     = m,
                                 lambda = l,
                                 nu     = n,
                                 kernel = k)$value
        return(integration)
    }

    cdf <- switch(kernel,
                  "normal"   = mapply(cumulative, q, l = lambda, m = mu, 
                                      n = 1.0, k = "normal"),
                  "t"        = mapply(cumulative, q, l = lambda, m = mu, 
                                      n = nu,  k = "t"),
                  "laplace"  = mapply(cumulative, q, l = lambda, m = mu, 
                                      n = 1.0, k = "laplace"),
                  "logistic" = mapply(cumulative, q, l = lambda, m = mu, 
                                      n = 1.0, k = "logistic")
           )

     if(lower.tail == FALSE){
         cdf <- (1 - cdf)
     }

     if(log.p == TRUE){
         cdf <- log(cdf)
     }
     return(cdf)
}

