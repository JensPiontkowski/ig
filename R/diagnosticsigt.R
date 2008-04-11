`diagnosticsigt` <-
function(x,
                           kernel    = "normal",
                           mainTitle = "",
                           yRange    = NULL){

    estimates <- switch(kernel,
                        "normal"   = mleigt(x, kernel = "normal"),
                        "t"        = mleigSt(x),
                        "laplace"  = mleigt(x, kernel = "laplace"),
                        "logistic" = mleigt(x, kernel = "logistic")
                 )

    parameters    <- c(estimates$muEstimate, estimates$lambdaEstimate)
    nuFinal       <- estimates$nuOptimal
    hessian       <- matrix(NA, 2, 2)
    hessian[1, 1] <- switch(kernel,
                            "normal"   = lmm(parameters, x, nu = 1.0,     
                                             "normal"),
                            "t"        = lmm(parameters, x, nu = nuFinal, 
                                             "t"),
                            "laplace"  = lmm(parameters, x, nu = 1.0,     
                                             "laplace"),
                            "logistic" = lmm(parameters, x, nu = 1.0,     
                                             "logistic")
                     )

    hessian[1, 2] <- switch(kernel,
                            "normal"   = lml(parameters, x, nu = 1.0,     
                                             "normal"),
                            "t"        = lml(parameters, x, nu = nuFinal, 
                                             "t"),
                            "laplace"  = lml(parameters, x, nu = 1.0,     
                                             "laplace"),
                            "logistic" = lml(parameters, x, nu = 1.0,     
                                             "logistic")
                     )

    hessian[2, 1] <- switch(kernel,
                            "normal"   = lml(parameters, x, nu = 1.0,     
                                             "normal"),
                            "t"        = lml(parameters, x, nu = nuFinal, 
                                             "t"),
                            "laplace"  = lml(parameters, x, nu = 1.0,     
                                             "laplace"),
                            "logistic" = lml(parameters, x, nu = 1.0,     
                                            "logistic")
                     )

    hessian[2, 2]  <- switch(kernel,
                             "normal"   = lll(parameters, x, nu = 1.0,    
                                              "normal"),
                             "t"        = lll(parameters, x, nu = nuFinal, 
                                              "t"),
                             "laplace"  = lll(parameters, x, nu = 1.0,     
                                              "laplace"),
                             "logistic" = lll(parameters, x, nu = 1.0,     
                                              "logistic")
                      )

    varcov   <- - solve(hessian)

    ## local influence
    n        <- length(x)
    Dm       <- vector("numeric")
    Dl       <- vector("numeric")
    argument <- kappaii(x, parameters)

    vi <- switch(kernel,
                 "normal"   = (- 2) * (wg(argument, nu = 1.0,     
                                          "normal")),
                 "t"        = (- 2) * (wg(argument, nu = nuFinal, 
                                          "t")),
                 "laplace"  = (- 2) * (wg(argument, nu = 1.0,     
                                          "laplace")),
                 "logistic" = (- 2) * (wg(argument, nu = 1.0,     
                                          "logistic"))
          )

    mu     <- parameters[1]
    lambda <- parameters[2]
    Dm     <- vi * ((lambda * (x - mu)) / (mu ^ 3))
    Dl     <- (1 / (2 * lambda)) - (1 / 2) * vi * ((x / (mu ^ 2)) + 
              (1 / x) - (2 / mu))

    Delta        <- rbind(Dm, Dl)
    B            <- t(Delta) %*% solve(hessian) %*% Delta
    EB           <- eigen(B)
    eigenvalues  <- eigen(B)$values
    eigenvectors <- eigen(B)$vectors

    ## Ci
    l <- 2 * abs(diag(B))
    ci <- 2 * mean(l)

    plot(l,
         type     = "h",
         main     = mainTitle,
         ylim     = yRange,
         xlab     = "Index",
         ylab     = expression(C[i]),
         cex.main = 1.5,
         las      = 1,
         col      = 4,
         lwd      = 1.5)
    lines(c(-10, n + 10), c(ci, ci), col = 2, lwd = 2.0)
}

