`mleigSt` <-
function(x){

    initials     <- mleig(x)
    thetaStart   <- c(initials$mu, initials$lambda)

    ## Choosing the value for nu from the data
    nus        <- seq(1, 100)
    resultsSic <- seq(1, 100)

    for(i in 1:100){
        nu            <- nus[i]
        resultsSic[i] <- sicigt(x, nu, "t")
    }

    ## What value for nu?
    minimum          <- min(resultsSic)
    degreesOfFreedom <- which(resultsSic == minimum)
    nuFinal          <- nus[degreesOfFreedom]

    logLik <- function(theta, x){

        sum(-digt(x, mu = theta[1], lambda = theta[2], nu = nuFinal,
            kernel = "t", log = TRUE))
    }
     
    maximization <- nlm(f = logLik, p = thetaStart, x = x)
    estimates    <- maximization$estimate
    results      <- list(muEstimate     = estimates[1],
                         lambdaEstimate = estimates[2],
                         nuOptimal       = nuFinal,
                         logLikelihood   = logLik(estimates, x))
    return(results)
}

