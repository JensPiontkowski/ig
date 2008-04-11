`mleigt` <-
function(x, kernel = "normal"){

    initials    <- mleig(x)
    thetaStart   <- c(initials$mu, initials$lambda)

    if(kernel == "normal"){

        logLik <- function(theta, x){

            sum(-digt(x, mu = theta[1], lambda = theta[2], nu = 1.0,
                kernel = "normal", log = TRUE))
        }
    }

    if(kernel == "logistic"){

        logLik <- function(theta, x){

            sum(-digt(x, mu = theta[1], lambda = theta[2], nu = 1.0,
                kernel = "logistic", log = TRUE))
        }
    }

    if(kernel == "laplace"){
        logLik <- function(theta, x){

            sum(-digt(x, mu = theta[1], lambda = theta[2], nu = 1.0,
                kernel = "laplace", log = TRUE))
        }
    }

    maximization <- nlm(f = logLik, p = thetaStart, x = x)
    estimates    <- maximization$estimate
    results      <- list(muEstimate     = estimates[1],
                         lambdaEstimate = estimates[2],
                         logLikelihood   = logLik(estimates, x))
    return(results)
}

