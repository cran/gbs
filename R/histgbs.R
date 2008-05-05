`histgbs` <-
function(x,
                    kernel          = "normal",
                    boxPlot         = "TRUE",
                    densityLine     = "FALSE",
                    mainTitle       = "Histogram and boxplot",
                    xLabel          = "Data",
                    yLabel          = "Frequency",
                    yRange          = NULL,
                    colourHistogram = "blue",
                    colourDensity   = "black",
                    colourBoxPlot   = "blue"){

    mat       <- matrix(c(1, 1, 2, 2), 2, 2, byrow = TRUE)
    nf        <- layout(mat, c(0.7, 7.0), c(7.0, 0.7), TRUE)
    layout.show(nf)
    par(mai = c(0, 0, 0, 0), mar = c(5, 5, 8, 5), mgp = c(4, 1, 0))
    minimum   <- min(x) - sd(x)
    maximum   <- max(x) + sd(x)
    axisx     <- seq(minimum, maximum, by = 0.1)
    estimates <- switch(kernel,
                        "normal"   = mlegbs(x, kernel = "normal"),
                        "t"        = mlebst(x),
                        "laplace"  = mlegbs(x, kernel = "laplace"),
                        "logistic" = mlegbs(x, kernel = "logistic")
                 )

    a       <- estimates$alphaEstimate
    b       <- estimates$betaEstimate
    z       <- estimates$nuOptimal
    valuesy <- switch(kernel,
                      "normal"   = dgbs(axisx,     alpha = a, beta = b, nu = 1.0,
                                           kernel = "normal"),
                      "t"        = dgbs(axisx,   alpha = a, beta = b, nu = z,
                                           kernel = "t"),
                      "laplace"  = dgbs(axisx, alpha = a, beta = b, nu = 1.0,
                                           kernel = "laplace"),
                      "logistic" = dgbs(axisx, alpha = a, beta = b, nu = 1.0,
                                           kernel = "logistic")
               )

    hist(x,
         freq     = FALSE,
         main     = mainTitle,
         xlab     = xLabel,
         ylab     = yLabel,
         ylim     = yRange,
         cex.main = 1.5,
         col      = colourHistogram,
         las      = 1)

    rug(x, side = 1)

    if(densityLine == "TRUE"){
        lines(axisx, valuesy, lwd = 2.0, col = colourDensity)
    }

    par(mar = c(0, 5, 0, 5))

    if(boxPlot == "TRUE"){
        boxplot.default(x, axes = FALSE, horizontal = TRUE,
                        notch = TRUE, col = colourBoxPlot)
    }
}

