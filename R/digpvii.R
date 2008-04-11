`digpvii` <-
function(x,
                    mu         = 1.0,
                    lambda     = 1.0,
                    parameters = c(1.0, 1.0),
                    log        = FALSE){

    dpvii <- function(u, theta = c(1.0, 1.0)){

        qParameter <- theta[1]
        rParameter <- theta[2]
        nc         <- gamma(qParameter) / 
                      (sqrt(rParameter * pi) * gamma(qParameter - (1 / 2)))
        gKernel    <- (1 + ((u ^ 2) / rParameter)) ^ (- qParameter)
        dsty       <- nc * gKernel
        return(dsty)
    }

    quantity <- sqrt(lambda / mu)*(sqrt(x / mu) - sqrt(mu / x))
    jacobian <- sqrt(lambda) / sqrt((x ^ 3))
    density  <- dpvii(quantity, parameters) * jacobian

    if(log == TRUE){
        density <- log(density)
    }
    return(density)
}

