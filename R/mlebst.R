`mlebst` <-
function(x){

    initials   <- mlebs(x)
    thetaStart <- c(initials$alphabsEstimate, initials$betabsEstimate)

    ## Choosing the value for nu from the data
    nus        <- seq(1, 100)
    resultsSic <- seq(1, 100)

    for(i in 1:100){
        nu            <- nus[i]
        resultsSic[i] <- sicgbs(x, nu, "t")
    }

    ## What value for nu?
    minimum          <- min(resultsSic)
    degreesOfFreedom <- which(resultsSic == minimum)
    nuFinal          <- nus[degreesOfFreedom]

    logLik <- function(theta, x){

        sum(-dgbs(x, alpha = theta[1], beta = theta[2], nu = nuFinal,
            kernel = "t", log = TRUE))
    }

    maximization <- nlm(f = logLik, p = thetaStart, x = x)
    estimates    <- maximization$estimate
    results      <- list(alphaEstimate = estimates[1],
                         betaEstimate  = estimates[2],
                         nuOptimal     = nuFinal,
                         logLikelihood = logLik(estimates, x))
    return(results)
}

