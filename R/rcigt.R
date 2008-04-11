`rcigt` <-
function(x, casesRemoved = NULL, kernel = "normal"){

    estimates <- switch(kernel,
                        "normal"   = mleigt(x, kernel = "normal"),
                       	"t"        = mleigSt(x),
                        "laplace"  = mleigt(x, kernel = "laplace"),
                        "logistic" = mleigt(x, kernel = "logistic")
                 )

    mu           <- estimates$muEstimate
    lambda       <- estimates$lambdaEstimate
    deleted      <- x[casesRemoved]
    newdata      <- x[-as.vector(casesRemoved)]

    newestimates <- switch(kernel,
                           "normal"   = mleigt(newdata, kernel = "normal"),
                           "t"        = mleigSt(newdata),
                           "laplace"  = mleigt(newdata, kernel = "laplace"),
                           "logistic" = mleigt(newdata, kernel = "logistic")
                    )

    newmu     <- newestimates$muEstimate
    newlambda <- newestimates$lambdaEstimate
    rcmu      <- round(abs(((mu - newmu) / mu)) * 100, 2)
    rclambda  <- round(abs(((lambda - newlambda) / lambda)) * 100, 2)
    results   <- list(casesRemoved = deleted,
                      muRelativechanges = rcmu,
                      lambdaRelativeChanges = rclambda)
    return(results)
}

