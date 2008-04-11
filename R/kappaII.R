`kappaii` <-
function(x, theta = c(1.0, 1.0)){

    mu     <- theta[1]
    lambda <- theta[2]
    value  <- ((lambda / mu) * ((x / mu) + (mu / x) - 2))
    return(value)
}

