`loglikgbs` <-
function(x, nu = 1.0, kernel = "normal"){

    estimates <- switch(kernel,
                        "normal"   = mlegbs(x, kernel = "normal"),
                        "t"        = mlebst(x),
                        "laplace"  = mlegbs(x, kernel = "laplace"),
                        "logistic" = mlegbs(x, kernel = "logistic"))

    alpha <- estimates$alphaEstimate
    beta  <- estimates$betaEstimate
    f     <- switch(kernel,
                     "normal"   = dgbs(x, alpha, beta, nu = 1.0,
                                       kernel = "normal", log = TRUE),
                     "t"        = dgbs(x, alpha, beta, nu = nu,
                                       kernel = "t", log = TRUE),
                     "laplace"  = dgbs(x, alpha, beta, nu = 1.0,
                                       kernel = "laplace", log = TRUE),
                     "logistic" = dgbs(x, alpha, beta, nu = 1.0,
                                       kernel = "logistic", log = TRUE)
              )

    logLikelihood <- sum(f)
    return(logLikelihood)
}

