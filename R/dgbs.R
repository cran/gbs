`dgbs` <-
function(x,
                 alpha  = 1.0,
                 beta   = 1.0,
                 nu     = 1.0,
                 kernel = "normal",
                 log    = FALSE){

    quantity <- (1 / alpha) * (sqrt(x / beta) - sqrt(beta / x))
    jacobian <- x^(-3/2)*(x + beta) / (2*alpha*sqrt(beta))
    density  <- switch(kernel,
                       "normal"   = dnorm(x = quantity, 0, 1) * jacobian,
                       "t"        = dt(x = quantity, df = nu) * jacobian,
                       "laplace"  = dlaplace(x = quantity)    * jacobian,
                       "logistic" = dlogis(x = quantity)      * jacobian
                )

    if(log == TRUE){
        density <- log(density)
    }
    return(density)
}

