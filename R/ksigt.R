`ksigt` <-
function(x,
                  kernel = "normal",
                  graph = FALSE,
                  mainTitle = "Cumulative distribution function",
                  xLabel = "data",
                  yLabel = "cdf"){

    estimates <- switch(kernel,
                        "normal"   = mleigt(x, kernel = "normal"),
                        "t"        = mleigSt(x),
                        "laplace"  = mleigt(x, kernel = "laplace"),
                        "logistic" = mleigt(x, kernel = "logistic")
                 )

    a  <- estimates$muEstimate
    b  <- estimates$lambdaEstimate
    z  <- estimates$nuOptimal
    ks <- switch(kernel,
                 "normal"   = ks.test(x, "pigt", mu = a, lambda = b, 
                                      nu = 1.0, kernel = "normal"),
                 "t"        = ks.test(x, "pigt", mu = a, lambda = b, 
                                      nu = z, kernel = "t"),
                 "laplace"  = ks.test(x, "pigt", mu = a, lambda = b, 
                                      nu = 1.0, kernel = "laplace"),
                 "logistic" = ks.test(x, "pigt", mu = a, lambda = b, 
                                      nu = 1.0, kernel = "logistic")
          )

    if(graph == TRUE){
        plot(ecdf(x),
             do.points  = FALSE,
             main       = mainTitle,
             xlab       = xLabel,
             ylab       = yLabel,
             lwd        = 2.0,
             col.01line = 3,
             las        = 1)
        minimum <- min(x)
        maximum <- max(x)
        u <- seq(minimum, maximum, by=0.1)
        y <- switch(kernel,
                    "normal"   = pigt(u, mu = a, lambda = b, nu = 1.0,
                                      kernel = "normal"),
                    "t"        = pigt(u, mu = a, lambda = b, nu = z,   
                                      kernel = "t"),
                    "laplace"  = pigt(u, mu = a, lambda = b, nu = 1.0, 
                                      kernel = "laplace"),
                    "logistic" = pigt(u, mu = a, lambda = b, nu = 1.0, 
                                      kernel = "logistic")
                    )
        lines(u, y, col = 2.0, lwd = 2.0)
    }
    return(ks)
}

