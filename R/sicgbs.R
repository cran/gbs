`sicgbs` <-
function(x, nu = 1.0, kernel = "normal"){

    n         <- length(x)
    p         <- 2

    estimates <- switch(kernel,
                        "normal"   = mlegbs(x, kernel = "normal"),
                        "t"        = mlebstNuFixed(x, nu),
                        "laplace"  = mlegbs(x, kernel = "laplace"),
                        "logistic" = mlegbs(x, kernel = "logistic")
                 )

    alpha <- estimates$alphaEstimate
    beta  <- estimates$betaEstimate

    f <- switch(kernel,
                "normal"   = dgbs(x, alpha, beta, nu = 1.0,
                                  kernel = "normal", log = TRUE),
                "t"        = dgbs(x, alpha, beta, nu,
                                  kernel = "t", log = TRUE),
                "laplace"  = dgbs(x, alpha, beta, nu = 1.0,
                                  kernel = "laplace", log=TRUE),
                "logistic" = dgbs(x, alpha, beta, nu = 1.0,
                                  kernel = "logistic", log=TRUE)
         )

    logLikelihood <- sum(f)
    sicResult     <- (-logLikelihood / n) + ((p / 2) * (log(n) / n))
    return(sicResult)
}

