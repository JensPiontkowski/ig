`mleig` <-
function(x){

    l         <- length(x) / sum((1 / x) - (1 / mean(x)))
    estimates <- list(muEstimate     = mean(x), 
                      lambdaEstimate = l)
    return(estimates)
}

