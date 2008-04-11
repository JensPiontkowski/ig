`ppigt` <-
function(x,
                  kernel = "normal",
                  line   = FALSE,
                  xLabel = "Empirical distribution function",
                  yLabel = "Theorical distribution function"){

    n         <- length(x)
    estimates <- switch(kernel,
                        "normal"   = mleigt(x, kernel = "normal"),
                        "t"        = mleigSt(x),
                        "laplace"  = mleigt(x, kernel = "laplace"),
                        "logistic" = mleigt(x, kernel = "logistic")
                 )

    a   <- estimates$muEstimate
    b   <- estimates$lambdaEstimate
    z   <- estimates$nuOptimal
    cdf <- switch(kernel,
                  "normal"   = pigt(x, mu = a, lambda = b, nu = 1.0, 
                                    kernel = "normal"),
                  "t"        = pigt(x, mu = a, lambda = b, nu = z,   
                                    kernel = "t"),
                  "laplace"  = pigt(x, mu = a, lambda = b, nu = 1.0, 
                                    kernel = "laplace"),
                  "logistic" = pigt(x, mu = a, lambda = b, nu = 1.0, 
                                    kernel = "logistic")
           )

    empprob <- sort(cdf)
    k       <- seq(1, n, by = 1)
    teoprob <- (k - 0.5) / n
    plot(empprob,
         teoprob,
         xlab = xLabel,
         ylab = yLabel,
         col  = 4,
         xlim = c(0, 1),
         ylim = c(0, 1),
         lwd  = 1.5)
    cd <- ((cor(empprob, teoprob)) ^ 2) * 100
    text(0.1, 1.0, as.expression(substitute(R^2 == r, list(r = cd))))
    result <- list(coefficientofdetermination = cd)
    if(line == TRUE){
        lines(c(0, 1), c(0, 1), col = 2, lwd = 2.0)
    }
    return(result)
}

