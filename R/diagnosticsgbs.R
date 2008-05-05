`diagnosticsgbs` <-
function(x,
                           kernel    = "normal",
                           mainTitle = "",
                           yRange    = NULL){

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

    hessian[2, 2]  <- switch(kernel,
                             "normal"   = lbb(parameters, x, nu = 1.0,
                                              "normal"),
                             "t"        = lbb(parameters, x, nu = nuFinal,
                                              "t"),
                             "laplace"  = lbb(parameters, x, nu = 1.0,
                                              "laplace"),
                             "logistic" = lbb(parameters, x, nu = 1.0,
                                              "logistic")
                      )

    varcov   <- - solve(hessian)

    ## local influence
    n        <- length(x)
    Da       <- vector("numeric")
    Db       <- vector("numeric")
    argument <- kappaii(x, parameters)

    vi <- switch(kernel,
                 "normal"   = (- 2) * (wg(argument, nu = 1.0,
                                          "normal")),
                 "t"        = (- 2) * (wg(argument, nu = nuFinal,
                                          "t")),
                 "laplace"  = (- 2) * (wg(argument, nu = 1.0,
                                          "laplace")),
                 "logistic" = (- 2) * (wg(argument, nu = 1.0,
                                          "logistic"))
          )

    alpha <- parameters[1]
    beta  <- parameters[2]
    Da    <- ((vi * (((x / beta) + (beta / x) - 2) / (alpha^3))) - (1 / alpha))
    Db    <- ((1 / (x + beta)) - (vi * (((1 / x) - (x / beta^2)) / (2 * alpha^2))) - (1 / (2 * beta)))


    Delta        <- rbind(Da, Db)
    B            <- t(Delta) %*% solve(hessian) %*% Delta
    EB           <- eigen(B)
    eigenvalues  <- eigen(B)$values
    eigenvectors <- eigen(B)$vectors

    ## Ci
    l  <- 2 * abs(diag(B))
    ci <- 2 * mean(l)

    plot(l,
         type     = "h",
         main     = mainTitle,
         ylim     = yRange,
         xlab     = "Index",
         ylab     = expression(C[i]),
         cex.main = 1.5,
         las      = 1,
         col      = 1,
         lwd      = 1.5)
    lines(c(-10, n + 10), c(ci, ci), col = 1, lwd = 2.0)
}

