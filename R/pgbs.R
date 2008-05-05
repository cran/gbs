`pgbs` <-
function(q,
                 alpha      = 1.0,
                 beta       = 1.0,
                 nu         = 1.0,
                 kernel     = "normal",
                 lower.tail = TRUE,
                 log.p      = FALSE){

    cumulative <- function(value, a, b, n, k){
        integration <- integrate(dgbs,
                                 lower  = 0,
                                 upper  = value,
                                 alpha  = a,
                                 beta   = b,
                                 nu     = n,
                                 kernel = k)$value
        return(integration)
    }

    cdf <- switch(kernel,
                  "normal"   = mapply(cumulative, q, a = alpha, b = beta,
                                      n = 1.0, k = "normal"),
                  "t"        = mapply(cumulative, q, a = alpha, b = beta,
                                      n = nu,  k = "t"),
                  "laplace"  = mapply(cumulative, q, a = alpha, b = beta,
                                      n = 1.0, k = "laplace"),
                  "logistic" = mapply(cumulative, q, a = alpha, b = beta,
                                      n = 1.0, k = "logistic")
           )

     if(lower.tail == FALSE){
         cdf <- (1 - cdf)
     }

     if(log.p == TRUE){
         cdf <- log(cdf)
     }
     return(cdf)
}

