`lml` <-
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

    a      <- ((x - mu) / (mu ^ 3))
    b      <- vi * a
    z      <- sum(b)
    d      <- ((lambda * (x - mu)) / (mu ^ 3)) * ((x / (mu ^ 2))+(1 / x) - 
              (2 / mu))
    e      <- vip * d
    f      <- sum(e)
    result <- (f + z)
    return(result)
}

