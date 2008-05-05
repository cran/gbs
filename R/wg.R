`wg` <-
function(u, nu = 1.0, kernel = "normal"){

    result <- switch(kernel,
                "normal"   = (- 1 / 2) * rep(1, length(u)),
                "t"        = (- 1 * (nu + 1)) / (2 * (nu + u)),
                "laplace"  = (- 1) / (2 * sqrt(u)),
                "logistic" = (- 1) * (tanh((sqrt(u)) / 2)) / (2 * sqrt(u))
              )
    return(result)
}

