`ppgbs` <-
function(x,
                  kernel = "normal",
                  line   = FALSE,
                  xLabel = "Empirical distribution function",
                  yLabel = "Theorical distribution function"){

    n         <- length(x)
    estimates <- switch(kernel,
                        "normal"   = mlegbs(x, kernel = "normal"),
                        "t"        = mlebst(x),
                        "laplace"  = mlegbs(x, kernel = "laplace"),
                        "logistic" = mlegbs(x, kernel = "logistic")
                 )

    a   <- estimates$alphaEstimate
    b   <- estimates$betaEstimate
    z   <- estimates$nuOptimal
    cdf <- switch(kernel,
                  "normal"   = pgbs(x, alpha = a, beta = b, nu = 1.0,
                                    kernel = "normal"),
                  "t"        = pgbs(x, alpha = a, beta = b, nu = z,
                                    kernel = "t"),
                  "laplace"  = pgbs(x, alpha = a, beta = b, nu = 1.0,
                                    kernel = "laplace"),
                  "logistic" = pgbs(x, alpha = a, beta = b, nu = 1.0,
                                    kernel = "logistic")
           )

    empprob <- sort(cdf)
    k       <- seq(1, n, by = 1)
    teoprob <- (k - 0.5) / n
    plot(empprob,
         teoprob,
         xlab = xLabel,
         ylab = yLabel,
         col  = 1,
         xlim = c(0, 1),
         ylim = c(0, 1),
         lwd  = 1.5)
    cd <- ((cor(empprob, teoprob)) ^ 2) * 100
    text(0.1, 1.0, as.expression(substitute(R^2 == r, list(r = cd))))
    result <- list(coefficientOfDetermination = cd)
    if(line == TRUE){
        lines(c(0, 1), c(0, 1), col = 1, lwd = 2.0)
    }
    return(result)
}

