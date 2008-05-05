`qgbs` <-
function(p,
                 alpha      = 1.0,
                 beta       = 1.0,
                 nu         = 1.0,
                 kernel     = "normal",
                 lower.tail = TRUE,
                 log.p      = FALSE){

    auxiliary <- function(p, alpha, beta, nu, kernel = "normal"){

        origin        <- p
        a             <- alpha
        b             <- beta
        n             <- nu
        kerneldensity <- switch(kernel,
                                "normal"   = "normal",
                                "t"        = "t",
                                "laplace"  = "laplace",
                                "logistic" = "logistic"
                         )
        f <- function(x, p = origin, alpha = a, beta = b, nu = n,
                      kernel = kerneldensity){

            results <- switch(kernel,
                              "normal"   = pgbs(x, alpha, beta, nu = 1.0,
                                                kernel = "normal")   - p,
                              "t"        = pgbs(x, alpha, beta, nu,
                                                kernel = "t")        - p,
                              "laplace"  = pgbs(x, alpha, beta, nu = 1.0,
                                                kernel = "laplace")  - p,
                              "logistic" = pgbs(x, alpha, beta, nu = 1.0,
                                                kernel = "logistic") - p
                       )
            return(results)
        }

        optimitation <- uniroot(f, interval = c(0.000001, 5000))
        pquantile    <- optimitation$root
        return(pquantile)
    }

    pquantiles <- switch(kernel,
                         "normal"   = mapply(auxiliary, p, alpha, beta,
                                             nu = 1.0, "normal"),
                         "t"        = mapply(auxiliary, p, alpha, beta,
                                             nu, "t"),
                         "laplace"  = mapply(auxiliary, p, alpha, beta,
                                             nu = 1.0, "laplace"),
                         "logistic" = mapply(auxiliary, p, alpha, beta,
                                             nu = 1.0, "logistic")
                  )
    return(pquantiles)
}

