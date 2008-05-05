`rcgbs` <-
function(x, casesRemoved = NULL, kernel = "normal"){

    estimates <- switch(kernel,
                        "normal"   = mlegbs(x, kernel = "normal"),
                        "t"        = mlebst(x),
                        "laplace"  = mlegbs(x, kernel = "laplace"),
                        "logistic" = mlegbs(x, kernel = "logistic")
                 )

    alpha   <- estimates$alphaEstimate
    beta    <- estimates$betaEstimate
    deleted <- x[casesRemoved]
    newdata <- x[-as.vector(casesRemoved)]

    newestimates <- switch(kernel,
                           "normal"   = mlegbs(newdata, kernel = "normal"),
                           "t"        = mlebst(newdata),
                           "laplace"  = mlegbs(newdata, kernel = "laplace"),
                           "logistic" = mlegbs(newdata, kernel = "logistic")
                    )

    newalpha <- newestimates$alphaEstimate
    newbeta  <- newestimates$betaEstimate
    rcalpha  <- round(abs(((alpha - newalpha) / alpha)) * 100, 2)
    rcbeta   <- round(abs(((beta - newbeta) / beta)) * 100, 2)
    results  <- list(casesRemoved = deleted,
                      alphaRelativechanges = rcalpha,
                      betaRelativeChanges  = rcbeta)
    return(results)
}

