`ksgbs` <-
function(x,
                  kernel = "normal",
                  graph = FALSE,
                  mainTitle = "Cumulative distribution function",
                  xLabel = "data",
                  yLabel = "cdf"){

    estimates <- switch(kernel,
                        "normal"   = mlegbs(x, kernel = "normal"),
                        "t"        = mlebst(x),
                        "laplace"  = mlegbs(x, kernel = "laplace"),
                        "logistic" = mlegbs(x, kernel = "logistic")
                 )

    a  <- estimates$alphaEstimate
    b  <- estimates$betaEstimate
    z  <- estimates$nuOptimal
    ks <- switch(kernel,
                 "normal"   = ks.test(x, "pgbs", alpha = a, beta = b,
                                      nu = 1.0, kernel = "normal"),
                 "t"        = ks.test(x, "pgbs", alpha = a, beta = b,
                                      nu = z, kernel = "t"),
                 "laplace"  = ks.test(x, "pgbs", alpha = a, beta = b,
                                      nu = 1.0, kernel = "laplace"),
                 "logistic" = ks.test(x, "pgbs", alpha = a, beta = b,
                                      nu = 1.0, kernel = "logistic")
          )

    if(graph == TRUE){
        plot(ecdf(x),
             do.points  = FALSE,
             main       = mainTitle,
             xlab       = xLabel,
             ylab       = yLabel,
             lwd        = 2.0,
             col.01line = 1,
             las        = 1)
        minimum <- min(x)
        maximum <- max(x)
        u <- seq(minimum, maximum, by=0.1)
        y <- switch(kernel,
                    "normal"   = pgbs(u, alpha = a, beta = b, nu = 1.0,
                                      kernel = "normal"),
                    "t"        = pgbs(u, alpha = a, beta = b, nu = z,
                                      kernel = "t"),
                    "laplace"  = pgbs(u, alpha = a, beta = b, nu = 1.0,
                                      kernel = "laplace"),
                    "logistic" = pgbs(u, alpha = a, beta = b, nu = 1.0,
                                      kernel = "logistic")
                    )
        lines(u, y, col = 8, lwd = 2.0)
    }
    return(ks)
}

