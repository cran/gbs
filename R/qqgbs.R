`qqgbs` <-
function(x,
                  kernel  = "normal",
                  line    = FALSE,
                  xLabel  = 'Empirical quantiles',
                  yLabel  = 'Theoretical quantiles'){

    estimates <- switch(kernel,
                        "normal"   = mlegbs(x, kernel = "normal"),
                        "t"        = mlebst(x),
                        "laplace"  = mlegbs(x, kernel = "laplace"),
                        "logistic" = mlegbs(x, kernel = "logistic")
                 )

    a <- estimates$alphaEstimate
    b <- estimates$betaEstimate
    z <- estimates$nuOptimal
    n <- length(x)
    k <- seq(1, n, by = 1)
    P <- (k - 0.5) / n

    invf <- switch(kernel,
                   "normal"   = qgbs(P, alpha = a, beta = b, nu = 1.0,
                                     kernel = "normal"),
                   "t"        = qgbs(P, alpha = a, beta = b, nu = z,
                                     kernel = "t"),
                   "laplace"  = qgbs(P, alpha = a, beta = b, nu = 1.0,
                                     kernel = "laplace"),
                   "logistic" = qgbs(P, alpha = a, beta = b, nu = 1.0,
                                     kernel="logistic")
            )

    quantile <- sort(x)
    plot(quantile, invf, xlab = xLabel, ylab = yLabel, col = 1, lwd = 1.5)
    cd <- ((cor(quantile, invf)) ^ 2) * 100
    text(median(quantile), max(invf),
         as.expression(substitute(R^2 == r, list(r = cd))))
    result <- list(coefficientOfDetermination = cd)

    if(line == TRUE){
        x1 <- as.numeric(quantile(x, 0.25))
        x2 <- as.numeric(quantile(x, 0.75))
        y1 <- switch(kernel,
                     "normal"   = qgbs(0.25, a, b, nu = 1.0,
                                       kernel = "normal"),
                     "t"        = qgbs(0.25, a, b, nu = z,
                                       kernel = "t"),
                     "laplace"  = qgbs(0.25, a, b, nu = 1.0,
                                       kernel = "laplace"),
                     "logistic" = qgbs(0.25, a, b, nu = 1.0,
                                       kernel = "logistic")
              )

        y2 <- switch(kernel,
                     "normal"   = qgbs(0.75, a, b, nu = 1.0,
                                       kernel = "normal"),
                     "t"        = qgbs(0.75, a, b, nu = z,
                                       kernel = "t"),
                     "laplace"  = qgbs(0.75, a, b, nu = 1.0,
                                       kernel = "laplace"),
                     "logistic" = qgbs(0.75, a, b, nu = 1.0,
                                       kernel = "logistic")
              )

        m         <- ((y2 - y1) / (x2 - x1))
        intercept <- y1 - (m * x1)
        abline(intercept, m, col = 1, lwd = 2.0)
    }
    return(result)
}

