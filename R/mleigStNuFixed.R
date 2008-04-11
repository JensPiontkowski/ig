`mleigStNuFixed` <-
function(x, nu = 1.0){

    initials   <- mleig(x)
    thetaStart <- c(initials$mu, initials$lambda)

    logLik <- function(theta, x){

        sum(-digt(x, mu = theta[1], lambda = theta[2], nu = nu, 
            kernel = "t", log = TRUE))
    }

    maximization <- nlm(f = logLik, p = thetaStart, x = x)
    estimates    <- maximization$estimate
    results      <- list(muEstimate     = estimates[1],
                         lambdaEstimate = estimates[2],
                         nuFixed         = nu,
                         logLikelihood   = logLik(estimates, x))
    return(results)
}

