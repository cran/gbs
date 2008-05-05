`wgp` <-
function(u, nu = 1.0, kernel = "normal"){

    result <- switch(kernel,
                     "normal"   = 0 * rep(1, length(u)),
                     "t"        = (nu + 1) / (2 * ((nu + u) ^ 2)),
                     "laplace"  = 1 / (4 * sqrt((u ^ 3))),
                     "logistic" = (((- 1) * sqrt(u)) + (sinh(sqrt(u)))) /
                                  (4 * sqrt((u ^ 3)) * (1 + cosh(sqrt(u))))
              )
    return(result)
}

