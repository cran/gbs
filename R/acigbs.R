`acigbs` <-
function(x,
                   kernel          = "normal",
                   confLevel       = 95,
                   chart           = c(NULL, NULL, NULL, NULL),
                   colourRegion    = 1,
                   colourEstimates = 2){

    ch1   <- chart[1]
    ch2   <- chart[2]
    ch3   <- chart[3]
    ch4   <- chart[4]
    gamma <- 1 - (confLevel / 100)

    ## chi-square percentile
    percentile <- round(qchisq(1 - gamma, 2), 2)

    ## ellipse
    ellipse <- function(A, m, const, k){

        r <- A[1, 2] / sqrt(A[1, 1] * A[2, 2])
        ## This builds a null matrix Q
        matrixq <- matrix(0, 2, 2)

        ## This transforms the unit circle in a ellipse
        matrixq[1, 1] <-  sqrt(A[1, 1] %*% (1 + r) / 2)
        matrixq[1, 2] <- -sqrt(A[1, 1] %*% (1 - r) / 2)
        matrixq[2, 1] <-  sqrt(A[2, 2] %*% (1 + r) / 2)
        matrixq[2, 2] <-  sqrt(A[2, 2] %*% (1 - r) / 2)

        ## This define angles for plotting
        gammaangle <- seq(0, by = (2 * pi) / k, length = k)

        ## This define coordinates of points on a unit circle
        coordinatesz <- cbind(cos(gammaangle), sin(gammaangle))

        ## This define coordinates of points on the ellipse
        coordinatesx <- t(m + const * matrixq %*% t(coordinatesz))
        return(coordinatesx)
    }

    # Points on the ellipse
    pointsellipse  <- 1000
    estimates <- switch(kernel,
                        "normal"   = mlegbs(x, kernel = "normal"),
                        "t"        = mlebst(x),
                        "laplace"  = mlegbs(x, kernel = "laplace"),
                        "logistic" = mlegbs(x, kernel = "logistic")
                 )

    parameters    <- c(estimates$alphaEstimate, estimates$betaEstimate)
    nuFinal       <- estimates$nuOptimal
    hessian       <- matrix(NA, 2, 2)
    hessian[1, 1] <- switch(kernel,
                            "normal"   = laa(parameters, x, nu = 1.0,
                                             "normal"),
                            "t"        = laa(parameters, x, nu = nuFinal,
                                             "t"),
                            "laplace"  = laa(parameters, x, nu = 1.0,
                                             "laplace"),
                            "logistic" = laa(parameters, x, nu = 1.0,
                                             "logistic")
                     )

    hessian[1, 2] <- switch(kernel,
                            "normal"   = lab(parameters, x, nu = 1.0,
                                             "normal"),
                            "t"        = lab(parameters, x, nu = nuFinal,
                                             "t"),
                            "laplace"  = lab(parameters, x, nu = 1.0,
                                            "laplace"),
                            "logistic" = lab(parameters, x, nu = 1.0,
                                            "logistic")
                     )

    hessian[2, 1] <- switch(kernel,
                            "normal"   = lab(parameters, x, nu = 1.0,
                                             "normal"),
                            "t"        = lab(parameters, x, nu = nuFinal,
                                             "t"),
                            "laplace"  = lab(parameters, x, nu = 1.0,
                                             "laplace"),
                            "logistic" = lab(parameters, x, nu = 1.0,
                                            "logistic")
                     )

    hessian[2, 2] <- switch(kernel,
                            "normal"   = lbb(parameters, x, nu = 1.0,
                                             "normal"),
                            "t"        = lbb(parameters, x, nu = nuFinal,
                                             "t"),
                            "laplace"  = lbb(parameters, x, nu = 1.0,
                                             "laplace"),
                            "logistic" = lbb(parameters, x, nu = 1.0,
                                             "logistic")
                     )

    varcov   <- -solve(hessian)
    variance <- varcov
    constant <- sqrt(percentile)
    m        <- parameters
    xx       <- ellipse(A = variance, m = parameters, const = constant,
                        k = pointsellipse)
    plot(xx[, 1],
         xx[, 2],
         type = "l",
         xlab = expression(alpha),
         ylab = expression(beta),
         xlim = c(ch1, ch2),
         ylim = c(ch3, ch4),
         lwd  = 2,
         col  = colourRegion)

    points(parameters[1], parameters[2], pch = 20,
           col = colourEstimates, lwd = 2)
    A <- xx
    betainterval  <- sort(c(round(min(A[, 2]), 2), round(max(A[, 2]), 2)))
    alphainterval <- sort(c(round(min(A[, 1]), 2), round(max(A[, 1]), 2)))
    results       <- list(alphaEstimate = estimates$alpha,
                          alphaAci      = alphainterval,
                           betaEstimate = estimates$beta,
                           betaAci      = betainterval)
    return(results)
}

