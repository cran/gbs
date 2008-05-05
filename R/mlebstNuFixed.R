`mlebstNuFixed` <-
function(x, nu = 1.0){

    initials   <- mlebs(x)
    thetaStart <- c(initials$alphabsEstimate, initials$betabsEstimate)

    logLik <- function(theta, x){

        sum(-dgbs(x, alpha = theta[1], beta = theta[2], nu = nu,
            kernel = "t", log = TRUE))
    }

    maximization <- nlm(f = logLik, p = thetaStart, x = x)
    estimates    <- maximization$estimate
    results      <- list(alphaEstimate = estimates[1],
                         betaEstimate  = estimates[2],
                         nuFixed       = nu,
                         logLikelihood = logLik(estimates, x))
    return(results)
}

