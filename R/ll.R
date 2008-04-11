`ll` <-
function(theta, x, nu = 1.0, kernel = "normal"){

    mu       <- theta[1]
    lambda   <- theta[2]
    argument <- kappaii(x, c(mu, lambda))
    vi       <- switch(kernel,
                  "normal"   = (- 2) * (wg(argument, nu = 1.0, "normal")),
                  "t"        = (- 2) * (wg(argument, nu,       "t")),
                  "laplace"  = (- 2) * (wg(argument, nu = 1.0, "laplace")),
                  "logistic" = (- 2) * (wg(argument, nu = 1.0, "logistic"))
                )

    n        <- length(x)
    a        <- ((x / (mu ^ 2)) + (1 / x)-(2 / mu))
    b        <- vi * a
    result   <- (n / (2 * lambda)) - ((1 / 2) * sum(b))
    return(result)
}

