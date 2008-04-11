`digkotz` <-
function(x,
                    mu         = 1.0,
                    lambda     = 1.0,
                    parameters = c(1.0, 1.0, 1.0),
                    log        = FALSE){

    dkotz <- function(u, theta = c(1.0, 1.0, 1.0)){

        qParameter <- theta[1]
        rParameter <- theta[2]
        sParameter <- theta[3]
        argument   <- ((2 * qParameter) - 1) / (2 * sParameter)
        nc         <- (sParameter * (rParameter ^ argument)) / gamma(argument)
        gKernel    <- (u ^ (2 * (qParameter - 1))) * 
                      exp(- rParameter * (u ^ (2 * sParameter)))
        dsty       <- nc * gKernel
        return(dsty)
    }

    quantity <- sqrt(lambda / mu) * (sqrt(x / mu) - sqrt(mu / x))
    jacobian <- sqrt(lambda) / sqrt((x ^ 3))
    density  <- dkotz(quantity, parameters) * jacobian

    if(log == TRUE){
        density <- log(density)
    }
    return(density)
}

