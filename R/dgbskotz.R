`dgbskotz` <-
function(x,
                    alpha      = 1.0,
                    beta       = 1.0,
                    parameters = c(1.0, 1.0, 1.0),
                    log        = FALSE){

    dkotz <- function(u, theta = c(1.0, 1.0, 1.0)){

        qParameter <- theta[1]
        rParameter <- theta[2]
        sParameter <- theta[3]
        argument   <- ((2 * qParameter) - 1) / (2 * sParameter)
        nc         <- (sParameter * (rParameter ^ argument)) / gamma(argument)
        gKernel    <- (u ^ (2 * (qParameter - 1))) *
                      exp(- rParameter * (u ^ (2 * sParameter)))
        dsty       <- nc * gKernel
        return(dsty)
    }

    quantity <- (1 / alpha) * (sqrt(x / beta) - sqrt(beta / x))
    jacobian <- x^(-3/2)*(x + beta) / (2*alpha*sqrt(beta))
    density  <- dkotz(quantity, parameters) * jacobian

    if(log == TRUE){
        density <- log(density)
    }
    return(density)
}

