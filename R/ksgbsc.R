`ksgbsc` <-
function(x,	
                   status,
                   kernel    = "normal",
                   graph     = FALSE,
		       mainTitle = "Cumulative distribution function",
		       xLabel    = "data",
		       yLabel    = "cdf"){

                    estimates <- switch(kernel,
                                        "normal" = mlegbsc(x, status),
                                        "t"      = mlegbstc(x, nuFixed, status))

    a  <- estimates$alphaEstimate
    b  <- estimates$betaEstimate
    z  <- nuFixed
    ks <- switch(kernel,
                 "normal" = ks.test(x, "pgbs", alpha = a, beta = b),                                     
                 "t"      = ks.test(x, "pgbs", alpha = a, beta = b, nu = z))

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
        u       <- seq(minimum, maximum, by=0.1)
        y       <- switch(kernel, 
                          "normal" = pgbs(u, alpha = a, beta = b, nu = 1.0,
                                          kernel = "normal"),
                           "t"     = pgbs(u, alpha = a, beta = b, nu = z,
                                          kernel = "t"))
        lines(u, y, col = 8, lwd = 2.0)
       } 
    return(ks)
   }

