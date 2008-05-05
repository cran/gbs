`mlegbs` <-
function(x, kernel = "normal"){

    initials   <- mlebs(x)
    thetaStart <- c(initials$alphabsEstimate, initials$betabsEstimate)

    if(kernel == "normal"){

        logLik <- function(theta, x){

            sum(-dgbs(x, alpha = theta[1], beta = theta[2], nu = 1.0,
                kernel = "normal", log = TRUE))
        }
    }

    if(kernel == "logistic"){

        logLik <- function(theta, x){

            sum(-dgbs(x, alpha = theta[1], beta = theta[2], nu = 1.0,
                kernel = "logistic", log = TRUE))
        }
    }

    if(kernel == "laplace"){
        logLik <- function(theta, x){

            sum(-dgbs(x, alpha = theta[1], beta = theta[2], nu = 1.0,
                kernel = "laplace", log = TRUE))
        }
    }

    maximization <- nlm(f = logLik, p = thetaStart, x = x)
    estimates    <- maximization$estimate
    results      <- list(alphaEstimate = estimates[1],
                         betaEstimate  = estimates[2],
                         logLikelihood = logLik(estimates, x))
    return(results)
}

