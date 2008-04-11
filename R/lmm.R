`lmm` <-
function(theta, x, nu = 1.0, kernel = "normal"){

    mu       <- theta[1]
    lambda   <- theta[2]
    argument <- kappaii(x, c(mu, lambda))

    vi <- switch(kernel,
                 "normal"   = (- 2) * (wg(argument, nu = 1.0, "normal")),
                 "t"        = (- 2) * (wg(argument, nu, "t")),
                 "laplace"  = (- 2) * (wg(argument, nu = 1.0, "laplace")),
                 "logistic" = (- 2) * (wg(argument, nu = 1.0, "logistic"))
          )

    vip <- switch(kernel,
                  "normal"   = (- 2) * (wgp(argument, nu = 1.0, "normal")),
                  "t"        = (- 2) * (wgp(argument, nu, "t")),
                  "laplace"  = (- 2) * (wgp(argument, nu = 1.0, "laplace")),
                  "logistic" = (- 2) * (wgp(argument, nu = 1.0, "logistic"))
           )

    a      <- (lambda * ((3 * x) - (2 * mu))) / (mu ^ 4)
    b      <- vi * a
    z      <- sum(b)
    d      <- ((lambda ^ 2)*((x - mu) ^ 2)) / (mu ^ 6)
    e      <- vip * d
    f      <- sum(e)
    result <- (((- 2) * f) - z)
    return(result)
}

